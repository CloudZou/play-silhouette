package com.mohiva.play.silhouette.api.auth

trait Result {

}

case class HandlerResult[+T](result: Result, data: Option[T] = None)
