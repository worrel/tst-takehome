import Problem2.allCombinablePromotionsSetBased
import Problem2.combinablePromotionsSetBased
import Problem2.allCombinablePromotionsListBased
import Problem2.combinablePromotionsListBased

case class Promotion(code: String, notCombinableWith: Seq[String])
case class PromotionCombo(promotionCodes: Seq[String])

object Problem2 {
    
    // set-based approach
    def longestComboSetBased(allPromotions: Seq[Promotion], includePromo: Option[String]) = {
        // preprocessing
        val exclusions = allPromotions.map(promo => promo.code -> promo.notCombinableWith.toSet).toMap
        val promoSet = allPromotions.map(_.code).toSet

        def buildCombos(acc: Set[Set[String]], remaining: Set[String]): Set[Set[String]] = {
            if remaining.isEmpty then
                acc
            else
                val promo = remaining.head
                val exclude = exclusions(promo)

                // add this promo to the combo and build w/ remaining promos after removing notCombinables
                val promosTake = buildCombos(acc.map(_ + promo), remaining -- exclude - promo)

                // get the intersection of the notCombinables and remaining promos
                val remExclude = (exclude & remaining)

                // build the symmetric combos w/ each of remaining notCombinables
                val promosLeave = remExclude.flatMap(ex => buildCombos(acc.map(_ + ex), remaining -- exclusions(ex) - ex))
                
                promosLeave ++ promosTake
        }

        // optionally filter the processing by a promo that must be included
        val (startAcc, startRem) = includePromo.fold((Set(Set.empty), promoSet))(incl => (Set(Set(incl)), promoSet -- exclusions(incl) - incl))
        
        // build the complete set of promos recursively
        buildCombos(startAcc, startRem).map(promo => PromotionCombo(promo.toSeq.sorted)).toSeq
    }

    def allCombinablePromotionsSetBased(allPromotions: Seq[Promotion]): Seq[PromotionCombo] = {
        longestComboSetBased(allPromotions, None)
    }

    def combinablePromotionsSetBased(promotionCode: String, allPromotions: Seq[Promotion]): Seq[PromotionCombo] = {
        longestComboSetBased(allPromotions, Some(promotionCode))
    }


    // list/sequential approach
    def longestComboListBased(acc: Seq[Set[String]], rem: Seq[Promotion], filter: Seq[String]): Seq[Set[String]] = rem match {
        case Nil => acc
        case head :: tail => {

            // get the combos that don't include the head promo
            val combosWithoutHead = longestComboListBased(acc, tail, filter)

            // if the head *is not already filtered*
            // add it to the accumulator & add it's notCombinables to the filter & recurse
            val combosWithHead = filter.find(f => head.code == f).fold({
                longestComboListBased(acc.map(_ + head.code), tail, head.notCombinableWith ++ filter)
            })(_ => Nil)

            // remove any subsets created by the non-head branch
            combosWithHead ++ combosWithoutHead.filterNot(nh => combosWithHead.exists(h => nh.subsetOf(h)))
        }
    }

    def allCombinablePromotionsListBased(allPromotions: Seq[Promotion]): Seq[PromotionCombo] = {
        longestComboListBased(Seq(Set.empty), allPromotions, Nil).map(combo => PromotionCombo(combo.toSeq))
    }

    def combinablePromotionsListBased(promotionCode: String, allPromotions: Seq[Promotion]): Seq[PromotionCombo] = {
        allPromotions.find(p => p.code == promotionCode).map(promo =>
            longestComboListBased(Seq(Set(promotionCode)), allPromotions, promo.notCombinableWith)
                .map(combo => PromotionCombo(combo.toSeq))
        ).getOrElse(Seq.empty)
    }

}

@main def Problem2Main(): Unit =
   val promotions = Seq(
        Promotion("P1", Seq("P3")),
        Promotion("P2", Seq("P4", "P5")),
        Promotion("P3", Seq("P1")),
        Promotion("P4", Seq("P2")), 
        Promotion("P5", Seq("P2"))
    )

    // NOTE: used the set-based approach here but the other yields identical results
    
    println("Problem 2 - allCombinablePromotions")
    println("-----------------------------------")
    allCombinablePromotionsSetBased(promotions).foreach(println)
    println()

    println("Problem 2 - combinablePromotions(P1)")
    println("------------------------------------")
    combinablePromotionsSetBased("P1", promotions).foreach(println)
    println()

    println("Problem 2 - combinablePromotions(P3)")
    println("------------------------------------")
    combinablePromotionsSetBased("P3", promotions).foreach(println)
    println()