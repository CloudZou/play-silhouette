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

import com.mohiva.play.silhouette.api.crypto.Crypter.Crypter
import javax.inject.Inject
import zio.{Has, Layer, Task, ZIO, ZLayer}


object AuthenticatorEncoder {
  type AuthenticatorEncoder = Has[Service]
  trait Service {
    /**
     * Encodes a string.
     *
     * @param data The data to encode.
     * @return The encoded data.
     */
    def encode(data: String): Task[String]

    /**
     * Decodes a string.
     *
     * @param data The data to decode.
     * @return The decoded data.
     */
    def decode(data: String): Task[String]
  }

  val base64: Layer[Nothing, Has[Service]] = ZLayer.succeed {
    new Service {
      /**
       * Encodes a string.
       *
       * @param data The data to encode.
       * @return The encoded data.
       */
      override def encode(data: String): Task[String] = Task.effect(Base64.encode(data))

      /**
       * Decodes a string.
       *
       * @param data The data to decode.
       * @return The decoded data.
       */
      override def decode(data: String): Task[String] = Task.effect(Base64.decode(data))
    }
  }

  val crypter: ZLayer[Crypter, Nothing, Has[Service]] = ZLayer.fromService { (crypter: Crypter.Service) =>
    new Service {
      val key = ???
      /**
       * Encodes a string.
       *
       * @param data The data to encode.
       * @return The encoded data.
       */
      override def encode(data: String): Task[String] = crypter.encrypt(key, data)

      /**
       * Decodes a string.
       *
       * @param data The data to decode.
       * @return The decoded data.
       */
      override def decode(data: String): Task[String] = crypter.decrypt(key, data)
    }
  }
}
