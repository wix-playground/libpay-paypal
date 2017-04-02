package com.wix.pay.paypal.iin

import org.specs2.mutable.SpecWithJUnit
import org.specs2.specification.Scope

class IINTest extends SpecWithJUnit {
  trait Ctx extends Scope {}

  "countryCodes" should {
    "return correct country code for known IINs" in new Ctx {
      IIN.countryCodes.get("458010") must beEqualTo(Some("IL"))
    }

    "return some country code for test IINs" in new Ctx {
      IIN.countryCodes.get("422222") must not(beEmpty) // 4222222222222
    }
  }
}
