package com.wix.pay.paypal


import com.paypal.api.payments._
import com.wix.pay.creditcard.CreditCard
import com.wix.pay.model.CurrencyAmount


trait PaypalGatewayHelper {
  def retrieveAccessToken(merchant: PaypalMerchant, bnCode: Option[String] = None): String

  def createAuthorize(creditCard: CreditCard, currencyAmount: CurrencyAmount): Payment

  def createSale(creditCard: CreditCard, currencyAmount: CurrencyAmount): Payment

  def submitPayment(accessToken: String, payment: Payment): Payment

  def createCapture(currencyAmount: CurrencyAmount): Capture

  def submitCapture(accessToken: String, authorizationId: String, capture: Capture): Capture

  def submitVoidAuthorization(accessToken: String, authorizationId: String): Authorization
}
