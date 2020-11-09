package com.mohiva.play.silhouette.api.services

import com.mohiva.play.silhouette.api.{Identity, LoginInfo}
import zio.{Has, Task}

object Identity {
  type IdentityService[T <: Identity] = Has[Service[T]]
  trait Service[T <: Identity] {
    def retrieve(loginInfo: LoginInfo): Task[Option[T]]
  }
}
