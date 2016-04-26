package com.wix.pay.paypal

import org.json4s.DefaultFormats
import org.json4s.native.Serialization

class JsonPaypalAuthorizationParser() extends PaypalAuthorizationParser {
  implicit val formats = DefaultFormats

  override def parse(authorizationKey: String): PaypalAuthorization = {
    Serialization.read[PaypalAuthorization](authorizationKey)
  }

  override def stringify(authorization: PaypalAuthorization): String = {
    Serialization.write(authorization)
  }
}
