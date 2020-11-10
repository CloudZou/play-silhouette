package com.mohiva.play.silhouette.settings

import com.typesafe.config.ConfigFactory
import play.api.Configuration
import zio.{Has, ZLayer}
import zio.blocking.Blocking

object ConfigurationProvider {

  type ConfigurationProvider = Has[Configuration]

  val live: ZLayer[Blocking, Throwable, ConfigurationProvider] = ZLayer.fromFunction(_ => Configuration(ConfigFactory.load()))
}
