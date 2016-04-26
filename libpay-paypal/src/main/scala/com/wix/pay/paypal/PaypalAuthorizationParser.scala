package com.wix.pay.paypal

trait PaypalAuthorizationParser {
  def parse(authorizationKey: String): PaypalAuthorization
  def stringify(authorization: PaypalAuthorization): String
}
