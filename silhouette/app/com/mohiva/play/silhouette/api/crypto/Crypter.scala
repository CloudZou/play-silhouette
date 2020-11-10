/**
 * Copyright 2015 Mohiva Organisation (license at mohiva dot com)
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
 */
package com.mohiva.play.silhouette.api.crypto

import java.security.MessageDigest
import java.util.Base64

import com.mohiva.play.silhouette.api.exceptions.CryptoException
import javax.crypto.Cipher
import javax.crypto.spec.{IvParameterSpec, SecretKeySpec}
import zio.{Has, Layer, Task, ZLayer}

object Crypter {
  type Crypter = Has[Service]

  trait Service {
    /**
     * Encrypts a string.
     *
     * @param value The plain text to encrypt.
     * @return The encrypted string.
     */
    def encrypt(key: String, value: String): Task[String]

    /**
     * Decrypts a string.
     *
     * @param value The value to decrypt.
     * @return The plain text string.
     */
    def decrypt(key: String, value: String): Task[String]
  }

  val UnderlyingIVBug = "[Silhouette][JcaCrypter] Cannot get IV! There must be a bug in your underlying JCE " +
    "implementation; The AES/CTR/NoPadding transformation should always provide an IV"
  val UnexpectedFormat = "[Silhouette][JcaCrypter] Unexpected format; expected [VERSION]-[ENCRYPTED STRING]"
  val UnknownVersion = "[Silhouette][JcaCrypter] Unknown version: %s"

  val live: Layer[Nothing, Has[Service]] = ZLayer.succeed {
    new Service {
      /**
       * Encrypts a string.
       *
       * @param value The plain text to encrypt.
       * @return The encrypted string.
       */
      override def encrypt(key: String,  value: String): Task[String] = Task.fromFunctionM { _ =>
        val keySpec = secretKeyWithSha256(key, "AES")
        val cipher = Cipher.getInstance("AES/CTR/NoPadding")
        cipher.init(Cipher.ENCRYPT_MODE, keySpec)
        val encryptedValue = cipher.doFinal(value.getBytes("UTF-8"))
        val version = 1
        Option(cipher.getIV) match {
          case Some(iv) => Task.succeed(s"$version-${Base64.getEncoder.encodeToString(iv ++ encryptedValue)}")
          case None     => Task.fail(new CryptoException(UnderlyingIVBug))
        }
      }

      /**
       * Decrypts a string.
       *
       * @param value The value to decrypt.
       * @return The plain text string.
       */
      override def decrypt(key: String,  value: String): Task[String] = Task.succeed {
        value.split("-", 2) match {
          case Array(version, data) if version == "1" => decryptVersion1(data, key)
          case Array(version, _)                      => throw new CryptoException(UnknownVersion.format(version))
          case v                                      => throw new CryptoException(UnexpectedFormat)
        }
      }

      /**
       * Generates the SecretKeySpec, given the private key and the algorithm.
       */
      private def secretKeyWithSha256(privateKey: String, algorithm: String) = {
        val messageDigest = MessageDigest.getInstance("SHA-256")
        messageDigest.update(privateKey.getBytes("UTF-8"))
        // max allowed length in bits / (8 bits to a byte)
        val maxAllowedKeyLength = Cipher.getMaxAllowedKeyLength(algorithm) / 8
        val raw = messageDigest.digest().slice(0, maxAllowedKeyLength)
        new SecretKeySpec(raw, algorithm)
      }

      /**
       * V1 decryption algorithm (AES/CTR/NoPadding - IV present).
       */
      private def decryptVersion1(value: String, privateKey: String): String = {
        val data = Base64.getDecoder.decode(value)
        val keySpec = secretKeyWithSha256(privateKey, "AES")
        val cipher = Cipher.getInstance("AES/CTR/NoPadding")
        val blockSize = cipher.getBlockSize
        val iv = data.slice(0, blockSize)
        val payload = data.slice(blockSize, data.size)
        cipher.init(Cipher.DECRYPT_MODE, keySpec, new IvParameterSpec(iv))
        new String(cipher.doFinal(payload), "UTF-8")
      }
    }
  }
}
