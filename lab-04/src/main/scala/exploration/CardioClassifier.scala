package exploration

import weka.classifiers.Classifier
import weka.classifiers.Evaluation
import weka.classifiers.bayes.NaiveBayes
import weka.classifiers.functions.SMO
import weka.classifiers.trees.{J48, RandomForest}
import weka.core.{Instances, Utils}
import weka.core.converters.CSVLoader
import weka.filters.Filter
import weka.filters.unsupervised.attribute.{Normalize, NumericToNominal, Remove, ReplaceMissingValues, Standardize}

import java.io.File
import java.util.{Objects, Random}

@main
def main(): Unit = {
  val config = ExperimentConfig()
  val data = DataPreprocessor.loadAndFilterData(config)

  printDataExploration(data)
  printDataPreparation()

  val (rawData, normalizedData, standardizedData) =
    DataPreprocessor.preprocessData(data)
  val experimentRunner = new ExperimentRunner(config)

  println("\n--- WYNIKI DLA DANYCH SUROWYCH ---")
  experimentRunner.runExperiments(rawData)

  println("\n--- WYNIKI DLA DANYCH ZNORMALIZOWANYCH ---")
  experimentRunner.runExperiments(normalizedData)

  println("\n--- WYNIKI DLA DANYCH STANDARYZOWANYCH ---")
  experimentRunner.runExperiments(standardizedData)

  runBonusExperiments(rawData)
}

case class ExperimentConfig(
    datasetPath: String = "cardiotocography_v2.csv",
    folds: Int = 10,
    randomSeed: Int = 1
)

case class ClassifierConfig(
    name: String,
    classifier: Classifier,
    description: String
)

object DataPreprocessor {
  def loadAndFilterData(config: ExperimentConfig): Instances = {
    val loader = new CSVLoader
    loader.setSource(
      new File(
        Objects
          .requireNonNull(
            getClass.getClassLoader.getResource(config.datasetPath)
          )
          .getFile
      )
    )
    var data = loader.getDataSet

    val attributesToKeep = Array(
      "LB",
      "AC",
      "FM",
      "UC",
      "DL",
      "DS",
      "DP",
      "ASTV",
      "MSTV",
      "ALTV",
      "MLTV",
      "Width",
      "Min",
      "Max",
      "Nmax",
      "Nzeros",
      "Mode",
      "Mean",
      "Median",
      "Variance",
      "Tendency",
      "CLASS"
    )
    val indicesToKeep =
      attributesToKeep.map(name => data.attribute(name).index() + 1)

    val removeFilter = new Remove()
    removeFilter.setAttributeIndices(indicesToKeep.mkString(","))
    removeFilter.setInvertSelection(true)
    removeFilter.setInputFormat(data)
    data = Filter.useFilter(data, removeFilter)

    val classIndex = data.attribute("CLASS").index()

    val converter = new NumericToNominal
    converter.setAttributeIndices(s"${classIndex + 1}")
    converter.setInputFormat(data)
    data = Filter.useFilter(data, converter)

    data.setClassIndex(classIndex)
    data
  }

  def preprocessData(data: Instances): (Instances, Instances, Instances) = {
    val replacer = new ReplaceMissingValues
    replacer.setInputFormat(data)
    val cleanData = Filter.useFilter(data, replacer)

    val normalizeFilter = new Normalize
    normalizeFilter.setInputFormat(cleanData)
    val normalizedData = Filter.useFilter(cleanData, normalizeFilter)

    val standardizeFilter = new Standardize
    standardizeFilter.setInputFormat(cleanData)
    val standardizedData = Filter.useFilter(cleanData, standardizeFilter)

    (cleanData, normalizedData, standardizedData)
  }
}

class ExperimentRunner(config: ExperimentConfig) {
  private def createClassifiers: List[ClassifierConfig] = List(
    ClassifierConfig("Naive Bayes", new NaiveBayes, "Naive Bayes"),
    ClassifierConfig("J48 (default)", new J48, "J48 (default: C=0.25, M=2)"),
    ClassifierConfig(
      "J48 (less pruning)", {
        val j48 = new J48
        j48.setConfidenceFactor(0.5f)
        j48
      },
      "J48 (less pruning: C=0.5, M=2)"
    ),
    ClassifierConfig(
      "J48 (more pruning)", {
        val j48 = new J48
        j48.setConfidenceFactor(0.1f)
        j48.setMinNumObj(10)
        j48
      },
      "J48 (more pruning: C=0.1, M=10)"
    ),
    ClassifierConfig("Random Forest", new RandomForest, "Random Forest"),
    ClassifierConfig("SVM", new SMO, "SVM (SMO)")
  )

  def runExperiments(data: Instances): Unit = {
    createClassifiers.foreach { config =>
      evaluateAndPrintResults(config.classifier, data, config.description)
    }
  }

  def evaluateAndPrintResults(
      classifier: Classifier,
      data: Instances,
      classifierName: String
  ): Unit = {
    println(s"\n--- Ocena dla: $classifierName ---")

    val eval = new Evaluation(data)
    eval.crossValidateModel(
      classifier,
      data,
      config.folds,
      new Random(config.randomSeed)
    )

    println(f"Dokładność (Accuracy): ${eval.pctCorrect()}%.4f%%")
    println(f"Precyzja (ważona):     ${eval.weightedPrecision}%.4f")
    println(f"Czułość (ważona):      ${eval.weightedRecall()}%.4f")
    println(f"F1-Score (ważony):     ${eval.weightedFMeasure}%.4f")
    println(s"\n${eval.toMatrixString("\n=== Macierz pomyłek ===\n")}")
  }
}

private def printDataExploration(data: Instances): Unit = {
  println(
    s"""
      |=========================================
      |KROK 1: EKSPLORACJA DANYCH (PO FILTROWANIU)
      |=========================================
      |Liczba instancji: ${data.numInstances()}
      |Liczba atrybutów: ${data.numAttributes()}
      |--- Podsumowanie zbioru danych ---
      |${data.toSummaryString}
      |""".stripMargin
  )
}

private def printDataPreparation(): Unit = {
  println(
    """
      |=========================================
      |KROK 2: PRZYGOTOWANIE DANYCH
      |=========================================
      |Dane zostaną przetworzone na 3 sposoby:
      |1. Dane surowe (po imputacji)
      |2. Dane znormalizowane
      |3. Dane standaryzowane
      |Do oceny modeli zostanie użyta walidacja krzyżowa (10-krotna).
      |""".stripMargin
  )
}

private def runBonusExperiments(data: Instances): Unit = {
  println(
    """
      |=========================================
      |BONUS: ŁAGODZENIE PRZEUCZENIA (J48)
      |=========================================
      |Porównanie drzewa bez przycinania (podatne na przeuczenie) z drzewem z agresywnym przycinaniem.
      |""".stripMargin
  )

  val experimentRunner = new ExperimentRunner(ExperimentConfig())

  val unprunedTree = new J48
  unprunedTree.setUnpruned(true)
  println("\n--- Drzewo J48 bez przycinania ---")
  experimentRunner.evaluateAndPrintResults(unprunedTree, data, "J48 (unpruned)")

  val prunedTree = new J48
  prunedTree.setConfidenceFactor(0.1f)
  prunedTree.setMinNumObj(5)
  println("\n--- Drzewo J48 z mocnym przycinaniem ---")
  experimentRunner.evaluateAndPrintResults(
    prunedTree,
    data,
    "J48 (pruned C=0.1, M=5)"
  )
}
