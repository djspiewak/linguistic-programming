import java.math.MathContext

object MonitaryUtils {
  trait Currency {
    def rateToUSD: BigDecimal
  }
  
  final class USD private[MonitaryUtils] () extends Currency {
    val rateToUSD = BigDecimal(1, MathContext.DECIMAL128)
  }

  val usd = new USD
  
  final class Euro private[MonitaryUtils] () extends Currency {
    val rateToUSD = BigDecimal(1.3895, MathContext.DECIMAL128)
  }
  
  val euro = new Euro
  
  
  case class Money[A <: Currency](amount: BigDecimal, currency: A) {
    def +(that: Money[A]) = Money(this.amount + that.amount, currency)
    
    def *(d: BigDecimal) = Money(amount * d, currency)
    
    def in[B <: Currency](c: B) = Money((amount * currency.rateToUSD) / c.rateToUSD, c)
  }
  
  
  implicit def doubleToCurrencySyntax(d: Double) = new {
    def usd = Money(BigDecimal(d, MathContext.DECIMAL128), MonitaryUtils.usd)
    def euro = Money(BigDecimal(d, MathContext.DECIMAL128), MonitaryUtils.euro)
  }
}


import MonitaryUtils._

def makePurchaseInEurope(from: Money[Euro]) = from


var balance = 123.42 usd

for (_ <- 0 to 10) {
  balance += balance * 0.01573
}

// ...

// balance = makePurchaseInEurope(balance)  // type fail

val euBalance = balance in euro
balance = makePurchaseInEurope(euBalance) in usd
