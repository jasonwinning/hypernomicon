/*
 * Copyright 2015-2026 Jason Winning
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.hypernomicon.util;

import java.nio.charset.StandardCharsets;
import java.security.*;
import java.security.spec.InvalidKeySpecException;
import java.util.*;
import java.util.concurrent.*;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.crypto.*;
import javax.crypto.spec.PBEKeySpec;
import javax.crypto.spec.PBEParameterSpec;

import org.hypernomicon.HyperTask;
import org.hypernomicon.model.Exceptions.CancelledTaskException;

import org.netbeans.api.annotations.common.NonNull;
import org.netbeans.api.annotations.common.NullAllowed;
import org.netbeans.api.keyring.Keyring;
import org.netbeans.spi.keyring.KeyringProvider;

import org.openide.util.*;

import static org.hypernomicon.App.app;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.Util.*;

import static org.netbeans.modules.keyring.utils.Utils.*;

import javafx.application.Platform;
import javafx.concurrent.Worker.State;

//---------------------------------------------------------------------------

public final class CryptoUtil
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private CryptoUtil() { throw new UnsupportedOperationException("Instantiation of utility class is not allowed."); }

//---------------------------------------------------------------------------

  private static final RequestProcessor keyringService = new RequestProcessor(Keyring.class);

  private static final String SECURITY_PROMPT_MESSAGE = "You might need to respond to a password prompt in a separate window.";

  /**
   * Delay before showing progress dialog for keyring operations.
   * Longer than default because OS credential prompts may take time to appear.
   */
  private static final long KEYRING_DIALOG_DELAY_MS = 750;

  private static KeyringProvider keyring;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static synchronized KeyringProvider getKeyring()
  {
    if (keyring == null)
    {
      Logger logger = Logger.getLogger(NbPreferences.class.getName());
      logger.setFilter(record -> record.getLevel().intValue() > Level.WARNING.intValue());

      keyring = Lookup.getDefault()
        .lookupAll(KeyringProvider.class).stream()
        .filter(KeyringProvider::enabled)
        .findFirst()
        .map(KeyringProvider.class::cast)
        .orElseGet(DummyKeyringProvider::new);
    }

    return keyring;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void logMessage(String message)
  {
    String providerMessage = "Keyring provider: " + nullSwitch(getKeyring(), "null", _keyring -> _keyring.getClass().getCanonicalName());

    System.out.println((providerMessage + ' ' + safeStr(message)).strip());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Reads a secret from the ring.
   * <p>
   * This method can be called from any thread.
   * All the changes done by previous calls to {@link #deleteFromKeyring(java.lang.String, java.lang.String)}
   * or {@link #saveToKeyring(java.lang.String, char[], java.lang.String, java.lang.String)} methods
   * are guaranteed to be visible by subsequent calls to this method.
   *
   * @param secretName the key for a secret key/value pair
   * @param taskMessage Description of work being done shown on progress dialog
   * @return its value if found (you may null out its elements), else null if not present
   * @since 1.31
   */
  public static char[] readFromKeyring(@NonNull final String secretName, String taskMessage)
  {
    Parameters.notNull("secretName", secretName);

    try
    {
      Future<char[]> futureResult = keyringService.submit(() -> getKeyring().read(secretName));

      return getResultWithProgressDialog(futureResult, "LoadFromKeyring", taskMessage, null);
    }
    catch (InterruptedException e)
    {
      Thread.currentThread().interrupt();
    }
    catch (ExecutionException e)
    {
      logMessage("Unable to read secret " + secretName + ": " + getThrowableMessage(e));
    }

    return null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

/**
 * Saves a secret to the ring.
 * If the key already existed, overwrites the password.
 * <p>
 * This method can be called from any thread.
 * The changes done by multiple calls to {@link #deleteFromKeyring(java.lang.String, java.lang.String)}
 * or {@link #readFromKeyring(java.lang.String, java.lang.String)} methods
 * are guaranteed to be processed in order in which they were called.
 *
 * @param secretName key for the secret key/value pair
 * @param secret the value for the secret key/value pair
 *                 (its contents will be nulled out by end of call)
 * @param description a user-visible description of the secret (may be null)
 * @param taskMessage Description of work being done shown on progress dialog
 * @return True if the secret was saved. False if an error was generated while saving, or if the
 *         wait for the save ended early (the user cancelled it, or the thread was interrupted),
 *         in which case the save may still complete.
 * @since 1.31
 */
  public static boolean saveToKeyring(@NonNull final String secretName, @NonNull final char[] secret, @NullAllowed final String description, String taskMessage)
  {
    Parameters.notNull("secretName", secretName);
    Parameters.notNull("secret", secret);

    Future<Boolean> futureResult = keyringService.submit(() ->
    {
      try
      {
        getKeyring().save(secretName, secret, description);
        return true;
      }
      catch (Exception e)
      {
        logMessage("Unable to save secret " + secretName + ": " + getThrowableMessage(e));
        return false;
      }
      finally
      {
        Arrays.fill(secret, (char) 0);
      }
    });

    // If the wait ends early, the outcome is unknown (the save may still complete), so report that
    // the secret cannot be assumed to be in the keyring. Wrongly assuming that only costs the caller
    // a redundant save; wrongly assuming the opposite would lose the secret.

    return finishWritingWithProgressDialog(futureResult, taskMessage, false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

/**
 * Deletes a secret from the ring.
 * If the secret was not in the ring to begin with, does nothing.
 * <p>
 * This method can be called from any thread.
 * The changes done by multiple calls to {@link #readFromKeyring(java.lang.String, java.lang.String)}
 * or {@link #saveToKeyring(java.lang.String, char[], java.lang.String, java.lang.String)} methods
 * are guaranteed to be processed in order in which they were called.
 *
 * @param secretName name for the secret
 * @param taskMessage Description of work being done shown on progress dialog
 * @return False if an error was generated while deleting; true otherwise, including when the
 *         wait for the delete ended early (the user cancelled it, or the thread was interrupted),
 *         in which case the delete may still complete.
 * @since 1.31
 */
  public static boolean deleteFromKeyring(@NonNull final String secretName, String taskMessage)
  {
    Parameters.notNull("secretName", secretName);

    Future<Boolean> futureResult = keyringService.submit(() ->
    {
      if (app.debugging)
        logMessage("Deleting secret: " + secretName);

      try
      {
        getKeyring().delete(secretName);
        return true;
      }
      catch (Exception e)
      {
        logMessage("Unable to delete secret " + secretName + ": " + getThrowableMessage(e));
        return false;
      }
    });

    // If the wait ends early, the outcome is unknown (the delete may still complete), so report
    // here too that the secret cannot be assumed to be in the keyring, which for a delete means true.

    return finishWritingWithProgressDialog(futureResult, taskMessage, true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Waits for a keyring write to finish and returns its result.
   *
   * @param taskMessage Description of work being done shown on progress dialog
   * @param valueIfOutcomeUnknown What to return if the wait ends before the write does, because
   *        the user cancelled the wait or the thread was interrupted
   * @return The result of the write; valueIfOutcomeUnknown if the wait ended early; false if the
   *         write threw an exception it did not handle itself
   */
  private static boolean finishWritingWithProgressDialog(Future<Boolean> futureResult, String taskMessage, boolean valueIfOutcomeUnknown)
  {
    try
    {
      return getResultWithProgressDialog(futureResult, "WriteToKeyring", taskMessage, valueIfOutcomeUnknown);
    }
    catch (InterruptedException e)
    {
      Thread.currentThread().interrupt();
      return valueIfOutcomeUnknown;
    }
    catch (ExecutionException e)
    {
      logMessage("Unable to write to keyring: " + getThrowableMessage(e));
      return false;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Waits for a keyring operation to finish and returns its result.
   * <p>
   * Off the FX thread, this blocks until the operation is done. On the FX thread, an operation
   * that is still running is waited for with a progress dialog that the user can cancel.
   * Cancelling only ends the wait; the operation itself still runs to completion.
   *
   * @param threadName Name of the thread that waits while the progress dialog is showing
   * @param taskMessage Description of work being done shown on progress dialog
   * @param valueIfCancelled What to return if the user cancels the wait
   * @return The result of the operation, or valueIfCancelled if the user cancelled the wait
   */
  private static <T> T getResultWithProgressDialog(Future<T> futureResult, String threadName, String taskMessage, T valueIfCancelled) throws InterruptedException, ExecutionException
  {
    // If not on FX thread, just wait for result

    if (Platform.isFxApplicationThread() == false)
      return futureResult.get();

    // If already done, return immediately

    if (futureResult.isDone())
      return futureResult.get();

    // Use HyperTask with built-in dialog delay to wait for result

    State state = new HyperTask(threadName, taskMessage, false) { @Override protected void call() throws CancelledTaskException
    {
      while (futureResult.isDone() == false)
      {
        sleepForMillis(100);
        throwExceptionIfCancelled(this);

        if (futureResult.isCancelled())
          throw new CancelledTaskException();
      }

    }}.setDialogDelayMillis(KEYRING_DIALOG_DELAY_MS).addMessage(SECURITY_PROMPT_MESSAGE).runWithProgressDialog();

    return state == State.SUCCEEDED ? futureResult.get() : valueIfCancelled;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * A simple implementation of the {@link KeyringProvider} interface
   * for managing in-memory storage of secrets, with no persistent storage.
   *
   * <p>This implementation uses a {@code Map<String, byte[]>}
   * to store secrets. Byte arrays are preferred to reduce the
   * readability of sensitive data in heap dumps.
   * @since 1.31
   */
  private static class DummyKeyringProvider implements KeyringProvider
  {
    // prefer byte[] to make secrets less readable in heap dumps:
    private final Map<String, byte[]> secrets = new HashMap<>();

  //---------------------------------------------------------------------------

    @Override public boolean enabled()                                   { return true; }
    @Override public void save(String key, char[] password, String desc) { secrets.put(key, chars2Bytes(password)); }
    @Override public void delete(String key)                             { secrets.remove(key); }

  //---------------------------------------------------------------------------

    @Override public char[] read(String key)
    {
      byte[] pwd = secrets.get(key);
      return pwd != null ? bytes2Chars(pwd) : null;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * @deprecated This constant is deprecated and will be removed in a future release. Use keyring functions instead.
   */
  @Deprecated
  private static final byte[] salt = { (byte) 0xA9, (byte) 0x9B, (byte) 0xC8, (byte) 0x32,
                                       (byte) 0x56, (byte) 0x35, (byte) 0xE3, (byte) 0x03 };
  /**
   * @deprecated This constant is deprecated and will be removed in a future release. Use keyring functions instead.
   */
  @Deprecated
  private static final int iterationCount = 19;

  /**
   * @deprecated This constant is deprecated and will be removed in a future release. Use keyring functions instead.
   */
  @Deprecated
  private static final String defaultK  = "pq0bJ2cYgC";

  /**
   * @deprecated This constant is deprecated and will be removed in a future release. Use keyring functions instead.
   */
  @Deprecated
  private static final String algorithm = "PBEWithMD5AndDES";

//---------------------------------------------------------------------------

  /**
   * This method is deprecated and will be removed in a future release.
   * Use {@link #saveToKeyring} instead.
   *
   * @deprecated
   */
  @Deprecated
  public static String encrypt(String secretKey, String plainText) throws NoSuchAlgorithmException, InvalidKeySpecException, NoSuchPaddingException, InvalidKeyException, InvalidAlgorithmParameterException, IllegalBlockSizeException, BadPaddingException
  {
    if (secretKey.isEmpty()) secretKey = defaultK;

    SecretKey key = SecretKeyFactory.getInstance(algorithm)
                                    .generateSecret(new PBEKeySpec(secretKey.toCharArray(), salt, iterationCount));

    Cipher ecipher = Cipher.getInstance(key.getAlgorithm());
    ecipher.init(Cipher.ENCRYPT_MODE, key, new PBEParameterSpec(salt, iterationCount));

    return new String(Base64.getEncoder().encode(ecipher.doFinal(plainText.getBytes(StandardCharsets.UTF_8))));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * This method is deprecated and will be removed in a future release.
   * Use {@link #readFromKeyring} instead.
   *
   * @deprecated
   */
  @Deprecated
  public static String decrypt(String secretKey, String encryptedText) throws NoSuchAlgorithmException, InvalidKeySpecException, NoSuchPaddingException, InvalidKeyException, InvalidAlgorithmParameterException, IllegalBlockSizeException, BadPaddingException
  {
    if (secretKey.isEmpty()) secretKey = defaultK;

    SecretKey key = SecretKeyFactory.getInstance(algorithm)
                                    .generateSecret(new PBEKeySpec(secretKey.toCharArray(), salt, iterationCount));

    Cipher dcipher = Cipher.getInstance(key.getAlgorithm());
    dcipher.init(Cipher.DECRYPT_MODE, key, new PBEParameterSpec(salt, iterationCount));

    return new String(dcipher.doFinal(Base64.getDecoder().decode(encryptedText)), StandardCharsets.UTF_8);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
