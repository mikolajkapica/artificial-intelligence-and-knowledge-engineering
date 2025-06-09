package exploration

import weka.classifiers.Classifier
import weka.classifiers.Evaluation
import weka.classifiers.bayes.NaiveBayes
import weka.classifiers.functions.SMO
import weka.classifiers.trees.J48
import weka.classifiers.trees.RandomForest
import weka.core.Instances
import weka.core.converters.CSVLoader
import weka.core.Utils
import weka.filters.Filter
import weka.filters.unsupervised.attribute.Normalize
import weka.filters.unsupervised.attribute.NumericToNominal
import weka.filters.unsupervised.attribute.Standardize
import weka.filters.unsupervised.attribute.ReplaceMissingValues
import java.io.File
import java.util.Objects
import java.util.Random

object CardioClassifier {
  private val DATASET_PATH = "cardiotocography_v2.csv"
  private val FOLDS = 10

  private def instances: Instances = {
    val loader = new CSVLoader
    loader.setSource(
      new File(Objects.requireNonNull(CardioClassifier.getClass.getClassLoader.getResource(DATASET_PATH)).getFile)
    )
    var data = loader.getDataSet
    val converter = new NumericToNominal
    converter.setAttributeIndices("last")
    converter.setInputFormat(data)
    data = Filter.useFilter(data, converter)

    if (data.classIndex == -1) data.setClassIndex(data.numAttributes - 1)

    data
  }

  def main(args: Array[String]): Unit = {
    var data = instances

    println(
      s"""
        |=========================================
        |KROK 1: EKSPLORACJA DANYCH
        |=========================================
        ||Liczba instancji: %d
        ||Liczba atrybutów: %d
        ||--- Podsumowanie zbioru danych ---
        |${data.toSummaryString}
        |""".stripMargin
    )

    println(
      """
        |=========================================
        |KROK 2: PRZYGOTOWANIE DANYCH
        |=========================================
        |Dane zostaną przetworzone na 3 sposoby:
        |1. Dane surowe (bez przetwarzania)
        |2. Dane znormalizowane
        |3. Dane standaryzowane
        |Do oceny modeli zostanie użyta walidacja krzyżowa (10-krotna).
        |""".stripMargin
    )

    // Sprawdzenie i uzupełnienie brakujących wartości
    val replacer = new ReplaceMissingValues
    replacer.setInputFormat(data)
    data = Filter.useFilter(data, replacer)

    // Przygotowanie przefiltrowanych wersji danych
    // Normalizacja
    val normalizeFilter = new Normalize
    normalizeFilter.setInputFormat(data)
    val normalizedData = Filter.useFilter(data, normalizeFilter)

    // Standaryzacja
    val standardizeFilter = new Standardize
    standardizeFilter.setInputFormat(data)
    val standardizedData = Filter.useFilter(data, standardizeFilter)

    println(
      """
        |=========================================
        |KROK 3 i 4: KLASYFIKACJA I OCENA
        |=========================================
        |""".stripMargin
    )

    println(
      """
        |--- WYNIKI DLA DANYCH SUROWYCH ---
        |""".stripMargin
    )
    runExperiments(data)

    println(
      """
        |--- WYNIKI DLA DANYCH ZNORMALIZOWANYCH ---
        |""".stripMargin
    )
    runExperiments(normalizedData)

    println(
      """
        |--- WYNIKI DLA DANYCH STANDARYZOWANYCH ---
        |""".stripMargin
    )
    runExperiments(standardizedData)

    println(
      """
        |=========================================
        |BONUS: ŁAGODZENIE PRZEUCZENIA (J48)
        |=========================================
        |Porównanie drzewa bez przycinania (podatne na przeuczenie) z drzewem z agresywnym przycinaniem.
        |""".stripMargin
    )

    val unprunedTree = new J48
    unprunedTree.setUnpruned(true)
    println(
      """
        |--- Drzewo J48 bez przycinania ---
        |""".stripMargin
    )
    evaluateAndPrintResults(unprunedTree, data, "J48 (unpruned)")

    val prunedTree = new J48
    prunedTree.setConfidenceFactor(0.1f) // Domyślnie 0.25

    prunedTree.setMinNumObj(5) // Domyślnie 2

    println(
      """
        |--- Drzewo J48 z mocnym przycinaniem ---
        |""".stripMargin
    )
    evaluateAndPrintResults(prunedTree, data, "J48 (pruned C=0.1, M=5)")
  }

  /** Uruchamia serię eksperymentów klasyfikacyjnych na danym zbiorze danych. */
  private def runExperiments(data: Instances): Unit = {
    // 1. Naiwny Klasyfikator Bayesa
    evaluateAndPrintResults(new NaiveBayes, data, "Naive Bayes")

    // 2. Drzewo decyzyjne J48 z różnymi hiperparametrami
    // Wariant 1: Domyślne parametry
    val j48_default = new J48
    evaluateAndPrintResults(j48_default, data, "J48 (default: C=0.25, M=2)")

    // Wariant 2: Mniej przycinania
    val j48_less_pruning = new J48
    j48_less_pruning.setConfidenceFactor(0.5f) // Większa ufność -> mniej przycinania

    evaluateAndPrintResults(j48_less_pruning, data, "J48 (less pruning: C=0.5, M=2)")

    // Wariant 3: Więcej przycinania
    val j48_more_pruning = new J48
    j48_more_pruning.setConfidenceFactor(0.1f) // Mniejsza ufność -> więcej przycinania

    j48_more_pruning.setMinNumObj(10) // Większa minimalna liczba obiektów w liściu

    evaluateAndPrintResults(j48_more_pruning, data, "J48 (more pruning: C=0.1, M=10)")

    // 3. Bonus: Bardziej zaawansowane algorytmy
    // Las losowy (Random Forest)
    evaluateAndPrintResults(new RandomForest, data, "Random Forest")

    // Maszyna wektorów nośnych (SVM)
    evaluateAndPrintResults(new SMO, data, "SVM (SMO)")
  }

  /** Ocenia dany klasyfikator na danym zbiorze danych przy użyciu walidacji krzyżowej i drukuje wyniki. */
  private def evaluateAndPrintResults(classifier: Classifier, data: Instances, classifierName: String): Unit = {
    println(
      s"""
        |--- Ocena dla: $classifierName ---
        |""".stripMargin
    )
    val eval = new Evaluation(data)

    // Używamy stałego ziarna losowego dla powtarzalności wyników
    eval.crossValidateModel(classifier, data, FOLDS, new Random(1))
    println(f"Dokładność (Accuracy): ${eval.pctCorrect()}%.4f%%\n")
    println(f"Precyzja (ważona):    ${eval.weightedPrecision}%.4f\n")
    println(f"Czułość (ważona):      ${eval.weightedRecall()}%.4f\n")
    println(f"F1-Score (ważony):     ${eval.weightedFMeasure}%.4f\n")

    // Drukowanie macierzy pomyłek
    println(
      s"""
        |${eval.toMatrixString("\n=== Macierz pomyłek ===\n")}
        |""".stripMargin
    )
  }
}
