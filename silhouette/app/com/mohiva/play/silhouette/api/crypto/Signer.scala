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

import com.mohiva.play.silhouette.api.exceptions.CryptoException
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec
import org.apache.commons.codec.binary.Hex
import zio.{Has, Layer, Task, ZLayer}

import scala.util.{Failure, Success, Try}

object Signer {
  type Signer = Has[Signer]

  trait Service {
    def sign(data: String): Task[String]

    def extract(message: String): Task[String]
  }

  val BadSignature = "[Silhouette][JcaSigner] Bad signature"
  val UnknownVersion = "[Silhouette][JcaSigner] Unknown version: %s"
  val InvalidMessageFormat = "[Silhouette][JcaSigner] Invalid message format; Expected [VERSION]-[SIGNATURE]-[DATA]"

  val live: Layer[Nothing, Has[Service]] = ZLayer.succeed {
    new Service {
      val key: String = ???
      val pepper = "-mohiva-silhouette-signer-"

      override def sign(data: String): Task[String] = {
        val message = pepper + data + pepper
        val mac = Mac.getInstance("HmacSHA1")
        mac.init(new SecretKeySpec(key.getBytes("UTF-8"), "HmacSHA1"))
        val signature = Hex.encodeHexString(mac.doFinal(message.getBytes("UTF-8")))
        val version = 1
        Task.succeed(s"$version-$signature-$data")
      }

      override def extract(message: String): Task[String] = {
        for {
          (_, actualSignature, actualData) <- fragment(message)
          ad <- sign(actualData)
          (_, expectedSignature, _) <- fragment(ad)
        } yield {
          if (constantTimeEquals(expectedSignature, actualSignature)) {
            actualData
          } else {
            throw new CryptoException(BadSignature)
          }
        }
      }

      private def fragment(message: String): Task[(String, String, String)] = {
        message.split("-", 3) match {
          case Array(version, signature, data) if version == "1" => Task.succeed((version, signature, data))
          case Array(version, _, _) => Task.fail(new CryptoException(UnknownVersion.format(version)))
          case _ => Task.fail(new CryptoException(InvalidMessageFormat))
        }
      }

      private def constantTimeEquals(a: String, b: String): Boolean = {
        if (a.length != b.length) {
          false
        } else {
          var equal = 0
          for (i <- 0 until a.length) {
            equal |= a(i) ^ b(i)
          }
          equal == 0
        }
      }
    }
  }
}
