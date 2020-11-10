package com.mohiva.play.silhouette.api

import com.mohiva.play.silhouette.api.crypto.AuthenticatorEncoder.AuthenticatorEncoder
import com.mohiva.play.silhouette.api.crypto.Crypter.Crypter
import com.mohiva.play.silhouette.api.crypto.Signer.Signer
import zio.blocking.Blocking

package object crypto {
  val live = AuthenticatorEncoder.crypter ++ AuthenticatorEncoder.base64 ++ Crypter.live ++ Signer.live

  type CryptoContext = Blocking with AuthenticatorEncoder with Crypter with Signer
}
