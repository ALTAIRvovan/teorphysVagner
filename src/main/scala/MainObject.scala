/**
  * Created by altair on 23.05.16.
  */
object MainObject {

    val sem_marks1part =  List(0, 1, 8, 4, 7, 3, 4, 0, 0, 5, 0, 0, 0, 6, 0, 1, 4, 0,   0, 1, 0, 1, 0, 2, 0, 2, 1, 13, 0, 2, 5).toArray
    val task_marks1part = List(1, 0, 4, 2, 2, 6, 3, 0, 0, 2, 2, 2, 2, 5, 0, 6, 2, 0,   3, 3, 0, 1, 0, 0, 3, 2, 4,  8, 1, 2, 5).toArray
    val kr_marks1part =   List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,   0, 0, 0, 0, 0, 0, 0, 0, 0,  0, 0, 0, 0).toArray
    val max_mark1part = 49 //Максимальный балл, который можно получить
    val max_kr_mark1 = 50

    val sem_marks2part =  List(7, 0, 4, 1, 3, 1, 0, 0, 0, 2, 0, 2, 4, 3, 3, 5, 3, 0,   2, 0, 0, 1, 0, 0, 1, 0, 0,  0, 0, 0, 0).toArray
    val task_marks2part = List(2, 2, 8, 2, 4, 3, 0, 0, 0, 4, 0, 1, 2, 4, 4, 3, 5, 0,   2, 4, 0, 5, 0, 1, 7, 3, 3,  9, 2, 2, 10).toArray
    val kr_marks2part =   List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,   0, 0, 0, 0, 0, 0, 0, 0, 0,  0, 0, 0, 0).toArray
    val max_mark2part = 62
    val max_kr_mark2 = 50

    //Количество студентов (надеюсь, что размеры всех массивов одинаковы)
    val QoS = sem_marks1part.length

    var k_sem1 = 1 //Коэффициент для баллов за семинары по первому заданию
    var k_sem2 = 2 // --//-- по второму заданию
    var k_semTotal = 3
    var k_kr = 4
    var k_com = 5 // Итоговый коэффициент


    var ac42: Double = - 1

    def calcAC42(coef: VagnerCoefficients): Double = {
        if(ac42 < 0) {
            val range = Array.ofDim[Double](QoS)
            for(i <- 0 until QoS) {
                range(i) = (max_mark1part - task_marks1part(i)) / (0.0001 + sem_marks1part(i))
            }
            ac42 = math.min(coef.k_sem1, range.min)
        }
        ac42
    }

    var ad42: Double = -1

    def calcAD42(coef: VagnerCoefficients): Double = {
        if(ad42 < 0) {
            val range = Array.ofDim[Double](QoS)
            for(i <- 0 until QoS) {
                range(i) = (max_mark2part - task_marks2part(i)) / (0.0001 + sem_marks2part(i))
            }
            ad42 = math.min(coef.k_sem2, range.min)
        }
        ad42
    }

    // Считает итоговый балл за первое задание
    // i    Номер студента
    // result Итоговый балл студента
    def calcFirstTaskResult(i: Int, coef: VagnerCoefficients): Double =
        calcAC42(coef) * sem_marks1part(i) + task_marks1part(i)
    def calcSecondTaskResult(i: Int, coef: VagnerCoefficients): Double  =
        calcAD42(coef) * sem_marks2part(i) + task_marks2part(i)

    def calcFinalRating(i: Int, coef: VagnerCoefficients): Double = {
        ((calcFirstTaskResult(i, coef) + calcSecondTaskResult(i, coef)) * coef.k_semTotal + (kr_marks1part(i) + kr_marks2part(i)) * coef.k_kr) /
            (coef.k_semTotal + 0.0001 + coef.k_kr)
    }

    def calcMaxRating(coef: VagnerCoefficients): Double = {
        ((max_mark1part + max_mark2part) * coef.k_semTotal + (max_kr_mark1 + max_kr_mark2) * coef.k_kr) /
            (coef.k_semTotal + 0.0001 + coef.k_kr)
    }

    def calcMarks(coef: VagnerCoefficients): Array[Int] = {
        val finalRating:Array[Double] = (0 until QoS).map((i: Int) => calcFinalRating(i, coef)).toArray
        val maxRatingValue = finalRating.max
        val maxPossibleRating = calcMaxRating(coef)
        val marks = Array.ofDim[Int](QoS)
        for(i <- 0 until QoS) {
            val a = 10 * finalRating(i) / maxRatingValue
            val b = 10 * finalRating(i) / maxPossibleRating
            val c = coef.k_com * finalRating(i) / maxRatingValue
            marks(i) = math.min(a , math.max(b, c)).toInt
        }
        marks
    }

    def F(coef: VagnerCoefficients):Double = {
        ac42 = -1
        ad42 = -1
        var average:Double = 0
        var mul: Double = 1
        val marks = calcMarks(coef)
        for(i <- marks ) {
            average += i.toDouble / QoS
        }
        var disp:Double = 0
        for(i <- marks) {
            disp += math.pow(i - average, 2) / QoS
        }
        marks.max / disp
    }

    def main(args: Array[String]): Unit ={
        val sym = new SimulateAnnealing(F)
        val coef = sym.solve(new VagnerCoefficients(k_sem1, k_sem2, k_semTotal, k_kr, k_com), 100, 1e-5)
        println(coef)
        println("F=" + F(coef))
        val marks = calcMarks(coef)
        println("Оценки:")
        marks.foreach(println)
        //(0 until QoS).foreach((i: Int) => println(calcFinalRating(i, coef)))
        //println("max_rating:" + calcMaxRating(coef))
        //println(ac42, ad42)
    }

}
