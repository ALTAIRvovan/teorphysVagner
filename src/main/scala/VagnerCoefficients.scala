/**
  * Created by altair on 24.05.16.
  */
class VagnerCoefficients(val k_sem1: Double, val k_sem2: Double, val k_semTotal: Double, val k_kr: Double, val k_com: Double) {
    override def toString: String = {
        "k_sem1:%f; k_sem2: %f; k_semTotal: %f; k_kr: %f; k_com: %f".format(k_sem1, k_sem2, k_semTotal, k_kr, k_com)
    }
}
