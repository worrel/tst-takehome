import Problem1.getBestGroupPrices

class Problem1Suite extends munit.FunSuite {
  test("empty inputs") {
    val rates: Seq[Rate] = Seq.empty
    val prices: Seq[CabinPrice] = Seq.empty
    
    val results = getBestGroupPrices(rates,prices)

    assertEquals(results.length, 0)
  }
  
  test("basic") {
    val rates: Seq[Rate] = List(
        Rate("AAA123","AAA"), 
        Rate("AAA223", "AAA"), 
        Rate("BBB123", "BBB"))

    val prices: Seq[CabinPrice] = List(
        CabinPrice("0001", "AAA123", 123.99), 
        CabinPrice("0001", "AAA223", 150.00), 
        CabinPrice("0002", "BBB123", 120.50))
    
    val bestPrices = getBestGroupPrices(rates,prices)
    assertEquals(bestPrices.length, 2)
    
    val bestPricesList = bestPrices.toIndexedSeq
    val price0 = bestPricesList(0)
    val price1 = bestPricesList(1)

    assertEquals(BestGroupPrice("0001", "AAA123", 123.99, "AAA"), price0)
    assertEquals(BestGroupPrice("0002", "BBB123", 120.50, "BBB"), price1)
  }

  test("provided") {
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
    assertEquals(bestPrices.length, 4)

    assertEquals(bestPrices.toList, List(
        BestGroupPrice("CA", "M1", 200.00, "Military"),
        BestGroupPrice("CA", "S1", 225.00, "Senior"),
        BestGroupPrice("CB", "M1", 230.00, "Military"),
        BestGroupPrice("CB", "S1", 245.00, "Senior"),
    ))
  }
}
