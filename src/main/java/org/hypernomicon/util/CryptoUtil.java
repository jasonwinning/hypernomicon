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

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.security.spec.AlgorithmParameterSpec;
import java.security.spec.InvalidKeySpecException;
import java.security.spec.KeySpec;
import java.util.Base64;

import javax.crypto.*;
import javax.crypto.spec.PBEKeySpec;
import javax.crypto.spec.PBEParameterSpec;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

public class CryptoUtil
{
  private static Cipher ecipher;
  private static Cipher dcipher;
  // 8-byte Salt
  private static byte[] salt = { (byte) 0xA9, (byte) 0x9B, (byte) 0xC8, (byte) 0x32, (byte) 0x56, (byte) 0x35, (byte) 0xE3, (byte) 0x03 };
  // Iteration count
  private static int iterationCount = 19;
  private static String defaultK = "pq0bJ2cYgC";

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
  
  /**
   *
   * @param secretKey
   *          Key used to encrypt data
   * @param plainText
   *          Text input to be encrypted
   * @return Returns encrypted text
   *
   */
  public static String encrypt(String secretKey, String plainText) throws NoSuchAlgorithmException, InvalidKeySpecException, NoSuchPaddingException, InvalidKeyException, InvalidAlgorithmParameterException, UnsupportedEncodingException, IllegalBlockSizeException, BadPaddingException
  {
    if (secretKey.length() == 0) secretKey = defaultK;
    
    // Key generation for enc and desc
    KeySpec keySpec = new PBEKeySpec(secretKey.toCharArray(), salt, iterationCount);
    SecretKey key = SecretKeyFactory.getInstance("PBEWithMD5AndDES").generateSecret(keySpec);
    
    // Prepare the parameter to the ciphers
    AlgorithmParameterSpec paramSpec = new PBEParameterSpec(salt, iterationCount);

    // Enc process
    ecipher = Cipher.getInstance(key.getAlgorithm());
    ecipher.init(Cipher.ENCRYPT_MODE, key, paramSpec);

    byte[] in = plainText.getBytes("UTF-8");
    byte[] out = ecipher.doFinal(in);
    
    return new String(Base64.getEncoder().encode(out));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
  
  /**
   * @param secretKey
   *          Key used to decrypt data
   * @param encryptedText
   *          encrypted text input to decrypt
   * @return Returns plain text after decryption
   */
  public static String decrypt(String secretKey, String encryptedText) throws NoSuchAlgorithmException, InvalidKeySpecException, NoSuchPaddingException, InvalidKeyException, InvalidAlgorithmParameterException, UnsupportedEncodingException, IllegalBlockSizeException, BadPaddingException, IOException
  {
    if (secretKey.length() == 0) secretKey = defaultK;
    
    // Key generation for enc and desc
    KeySpec keySpec = new PBEKeySpec(secretKey.toCharArray(), salt, iterationCount);
    SecretKey key = SecretKeyFactory.getInstance("PBEWithMD5AndDES").generateSecret(keySpec);
    
    // Prepare the parameter to the ciphers
    AlgorithmParameterSpec paramSpec = new PBEParameterSpec(salt, iterationCount);
    
    // Decryption process; same key will be used for decr
    dcipher = Cipher.getInstance(key.getAlgorithm());
    dcipher.init(Cipher.DECRYPT_MODE, key, paramSpec);
    
    byte[] enc = Base64.getDecoder().decode(encryptedText);
    byte[] utf8 = dcipher.doFinal(enc);
    
    String plainStr = new String(utf8, "UTF-8");
    return plainStr;
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
  
}
