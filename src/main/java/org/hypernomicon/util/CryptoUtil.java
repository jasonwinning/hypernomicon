/*
 * Copyright 2015-2019 Jason Winning
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

import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.security.spec.InvalidKeySpecException;
import java.util.Base64;

import javax.crypto.*;
import javax.crypto.spec.PBEKeySpec;
import javax.crypto.spec.PBEParameterSpec;

import static java.nio.charset.StandardCharsets.*;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

public class CryptoUtil
{
  private static byte[] salt = { (byte) 0xA9, (byte) 0x9B, (byte) 0xC8, (byte) 0x32, (byte) 0x56, (byte) 0x35, (byte) 0xE3, (byte) 0x03 };
  private static int iterationCount = 19;
  private static String defaultK  = "pq0bJ2cYgC",
                        algorithm = "PBEWithMD5AndDES";

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String encrypt(String secretKey, String plainText) throws NoSuchAlgorithmException, InvalidKeySpecException, NoSuchPaddingException, InvalidKeyException, InvalidAlgorithmParameterException, IllegalBlockSizeException, BadPaddingException
  {
    if (secretKey.length() == 0) secretKey = defaultK;

    SecretKey key = SecretKeyFactory.getInstance(algorithm)
                                    .generateSecret(new PBEKeySpec(secretKey.toCharArray(), salt, iterationCount));

    Cipher ecipher = Cipher.getInstance(key.getAlgorithm());
    ecipher.init(Cipher.ENCRYPT_MODE, key, new PBEParameterSpec(salt, iterationCount));

    return new String(Base64.getEncoder().encode(ecipher.doFinal(plainText.getBytes(UTF_8))));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String decrypt(String secretKey, String encryptedText) throws NoSuchAlgorithmException, InvalidKeySpecException, NoSuchPaddingException, InvalidKeyException, InvalidAlgorithmParameterException, IllegalBlockSizeException, BadPaddingException
  {
    if (secretKey.length() == 0) secretKey = defaultK;

    SecretKey key = SecretKeyFactory.getInstance(algorithm)
                                    .generateSecret(new PBEKeySpec(secretKey.toCharArray(), salt, iterationCount));

    Cipher dcipher = Cipher.getInstance(key.getAlgorithm());
    dcipher.init(Cipher.DECRYPT_MODE, key, new PBEParameterSpec(salt, iterationCount));

    return new String(dcipher.doFinal(Base64.getDecoder().decode(encryptedText)), UTF_8);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
