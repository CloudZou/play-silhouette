package com.mohiva.play.silhouette.api.util

import java.security.SecureRandom

import com.google.common.io.BaseEncoding
import com.mohiva.play.silhouette.api.crypto.Hash
import zio.{Has, Layer, Task, ZLayer}

object Generator {

  val live = FingerprintGenerator.live ++ IDGenerator.live

  object FingerprintGenerator {
    type FingerprintGenerator = Has[Service]

    trait Service {
      def generate(headers: Map[String, String]): Task[String]
    }

    val live: Layer[Nothing, Has[Service]] = ZLayer.succeed {
      (headers: Map[String, String]) => Task.succeed(Hash.sha1(headers.values.mkString(":")))
    }
  }


  object IDGenerator {
    type IDGenerator = Has[Service]

    trait Service {
      def generate: Task[String]
    }

    val live: Layer[Nothing, Has[Service]] = ZLayer.succeed {
      new Service {
        override def generate: Task[String] = {
          val randomValue = new Array[Byte](1090)
          val random = new SecureRandom()
          Task.succeed(random.nextBytes(randomValue)).map { _ =>
            BaseEncoding.base16.lowerCase.encode(randomValue)
          }
        }
      }
    }
  }

}
