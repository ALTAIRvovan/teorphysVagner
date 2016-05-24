/**
  * Created by altair on 24.05.16.
  */
class SimulateAnnealing(F: (VagnerCoefficients) => Double) {

    def decreaseTemperature( initialTemperature: Double, i : Int): Double = {
        initialTemperature / i
    }

    def decreaseTemperature2(T: Double): Double = T * 0.99

    def getTransitionProbability( dE: Double, T: Double): Double = {
        math.exp(-math.abs(dE) / T)
    }

    def isTransition(propobility: Double): Boolean = {
        val value = math.random
        if(value < propobility)
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
        var currentCoef:VagnerCoefficients = coef
        var currentEnergy:Double = F(coef)
        var T:Double = initialTemperature
        while (T > endTemperature) {
            val condidateCoef:VagnerCoefficients = getNextCoefficients(currentCoef, T)
            val condidateEnergy:Double = F(condidateCoef)
            //println(condidateEnergy)
            if(condidateEnergy > currentEnergy) {
                currentEnergy = condidateEnergy
                currentCoef = condidateCoef
            } else {
                val p = getTransitionProbability(currentEnergy - condidateEnergy, T)
                //println("p=" + p, "T=" + T, "dE=" + (currentEnergy - condidateEnergy).toDouble)
                if(isTransition(p)) {
                    currentEnergy = condidateEnergy
                    currentCoef = condidateCoef
                }
            }
            T = decreaseTemperature2(T)
        }
        currentCoef
    }
}
