import Problem1.getBestGroupPrices
case class Rate(rateCode: String, rateGroup: String)
case class CabinPrice(cabinCode: String, rateCode: String, price: BigDecimal)
case class BestGroupPrice(cabinCode: String, rateCode: String, price: BigDecimal, rateGroup: String)
case class BestPriceKey(rateGroup: String, cabinCode: String)

object Problem1 {
    def getBestGroupPrices(rates: Seq[Rate], prices: Seq[CabinPrice]): Seq[BestGroupPrice] = {

        val rateCodeToGroup = rates.view.map(rate => rate.rateCode -> rate.rateGroup).toMap

        prices.view.foldLeft(Map[BestPriceKey, BestGroupPrice]())((bestPrices, cabinPrice) => {
            rateCodeToGroup.get(cabinPrice.rateCode).map(rateGroup => {
                val key = BestPriceKey(rateGroup, cabinPrice.cabinCode)
                val bestPrice = bestPrices.get(key)
                    .filter(currBest => currBest.price < cabinPrice.price)
                    .getOrElse(BestGroupPrice(cabinPrice.cabinCode, cabinPrice.rateCode, cabinPrice.price, rateGroup))
                bestPrices.updated(key, bestPrice)
            }).getOrElse(bestPrices) // TODO: error reporting on missing rateCode mapping
        }).values.toSeq
    }
}

@main def Problem1Main(): Unit =
    val rates = List(
        Rate("M1", "Military"),
        Rate("M2", "Military"),
        Rate("S1", "Senior"),
        Rate("S2", "Senior")
    )
    val prices = List(
        CabinPrice("CA", "M1", 200.00),
        CabinPrice("CA", "M2",250.00),
        CabinPrice("CA", "S1", 225.00),
        CabinPrice("CA", "S2", 260.00),
        CabinPrice("CB", "M1", 230.00),
        CabinPrice("CB", "M2", 260.00),
        CabinPrice("CB", "S1", 245.00),
        CabinPrice("CB", "S2", 270.00)
    )

    val bestPrices = getBestGroupPrices(rates,prices)

    println("Problem 1 - BestGroupPrices")
    println("---------------------------")
    bestPrices.foreach(println)

