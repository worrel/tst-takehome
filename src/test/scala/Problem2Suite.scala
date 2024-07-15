import Problem2.allCombinablePromotionsSetBased
import Problem2.allCombinablePromotionsListBased

class Problem2Suite extends munit.FunSuite {
    
    test("solo only") {
        val promotions = Seq(
            Promotion("P1", Seq("P2","P3","P4","P5","P6","P7")),
            Promotion("P2", Seq("P1","P3","P4","P5","P6","P7")),
            Promotion("P3", Seq("P1","P2","P4","P5","P6","P7")),
            Promotion("P4", Seq("P1","P2","P3","P5","P6","P7")), 
            Promotion("P5", Seq("P1","P2","P3","P4","P6","P7")),
            Promotion("P6", Seq("P1","P2","P3","P4","P5","P7")),
            Promotion("P7", Seq("P1","P2","P3","P4","P5","P6"))
        )

        val combos = allCombinablePromotionsListBased(promotions)
        
        assertEquals(combos.length, 7)
        combos.foreach(combo => assertEquals(combo.promotionCodes.length, 1))
    }

    test("only two") {
        val promotions = Seq(
            Promotion("P1", Seq("P2")),
            Promotion("P2", Seq("P1")),
            Promotion("P3", Seq.empty),
            Promotion("P4", Seq.empty), 
            Promotion("P5", Seq.empty),
            Promotion("P6", Seq.empty),
            Promotion("P7", Seq.empty)
        )

        val combos = allCombinablePromotionsListBased(promotions)
        
        assertEquals(combos.length, 2)
        combos.foreach(combo => assertEquals(combo.promotionCodes.length, 6))
    }

    test("provided") {
        val promotions = Seq(
            Promotion("P1", Seq("P3")),
            Promotion("P2", Seq("P4", "P5")),
            Promotion("P3", Seq("P1")),
            Promotion("P4", Seq("P2")), 
            Promotion("P5", Seq("P2"))
        )

        val combos = allCombinablePromotionsSetBased(promotions)
        
        assertEquals(combos,
            Seq(
                PromotionCombo(Seq("P1", "P2")), 
                PromotionCombo(Seq("P1", "P4", "P5")), 
                PromotionCombo(Seq("P2", "P3")), 
                PromotionCombo(Seq("P3", "P4", "P5"))
            )
        )
    }
}