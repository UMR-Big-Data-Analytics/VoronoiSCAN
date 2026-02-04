package configuration

import singletons.AbstractGenericSingleton

object OutputConfiguration extends AbstractGenericSingleton[OutputConfiguration]() {

  set(OutputConfiguration())

}

case class OutputConfiguration(outputPath: String = "")
