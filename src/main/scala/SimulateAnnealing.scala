/**
  * Created by altair on 24.05.16.
  */
class SimulateAnnealing(F: (VagnerCoefficients) => Double) {

    def decreaseTemperature( initialTemperature: Double, i : Int): Double = {
        initialTemperature * 0.1 / i
    }

    def getTransitionProbability( dE: Double, T: Double): Double = {
        math.exp(-dE / T)
    }

    def isTransition(propobility: Double): Boolean = {
        val value = math.random
        if(value <= propobility)
            true
        else
            false
    }

    def getNextCoefficients(coef: VagnerCoefficients, T: Double): VagnerCoefficients = {
        new VagnerCoefficients(
            math.abs(coef.k_sem1 + (math.random - 0.5) * T),
            math.abs(coef.k_sem2 + (math.random - 0.5) * T),
            math.abs(coef.k_semTotal + (math.random - 0.5) * T),
            math.abs(coef.k_kr + (math.random - 0.5) * T),
            math.abs(coef.k_com + (math.random - 0.5) * T)
        )
    }

    def solve(coef: VagnerCoefficients, initialTemperature: Double, endTemperature: Double): VagnerCoefficients = {
        var currentCoef = coef
        var currentEnergy = F(coef)
        var T:Double = initialTemperature
        for (i <- 1 to 1000000 if T > endTemperature) {
            val condidateCoef = getNextCoefficients(currentCoef, T)
            val condidateEnergy = F(condidateCoef)
            //println(condidateEnergy)
            if(condidateEnergy > currentEnergy) {
                currentEnergy = condidateEnergy
                currentCoef = condidateCoef
            } else {
                val p = getTransitionProbability(currentEnergy - condidateEnergy, T)
                //println("p=" + p, "T=" + T, "dE=" + (currentEnergy - condidateEnergy))
                if(isTransition(p)) {
                    currentEnergy = condidateEnergy
                    currentCoef = condidateCoef
                }
            }
            T = decreaseTemperature(initialTemperature, i)
        }
        currentCoef
    }
}
