package lthv.ccs

import org.scalatest.funsuite.AnyFunSuite

class ValidatorTest extends AnyFunSuite {

  def getFilePath(fileName: String): String = s"src/test/resources/$fileName"

  def passes(fileName: String): Unit = {

    val errors = Validator.validate(getFilePath(fileName))

    assert(errors.isEmpty)
  }

  def fails(fileName: String): Unit = {

    val errors = Validator.validate(getFilePath(fileName))

    assert(errors.nonEmpty)
  }

  test("ISOD 1 test passes") {
    passes("isod_1")
  }

  test("ISOD 2 test fails") {
    fails("isod_2")
  }

  test("ISOD 2 test fails when indentation issues are resolved") {
    fails("isod_3")
  }

  test("Indentation done with spaces fails") {
    fails("space_indentation")
  }

  test("Improper indentation done with tabs fails") {
    fails("improper_tab_indentation")
  }

  test("Proper indentation passes") {
    passes("proper_indentation")
  }

  test("Empty properties fail") {
    fails("empty_properties")
  }

  test("gt selector passes") {
    passes("gt_selector")
  }

  test("complex selector passes") {
    passes("complex_selector")
  }

  test("multiline complex selector passes") {
    passes("multiline_complex_selector")
  }

  test("star passes") {
    passes("star")
  }

  test("complex class passes") {
    passes("complex_class")
  }

  test("underscore class passes") {
    passes("underscore_class")
  }

  test("last property without semicolon passes") {
    passes("last_property_no_semi")
  }

  test("no semicolon in non-last property fails") {
    fails("no_semi_in_non_last")
  }

  test("color property passes") {
    passes("color_property")
  }

  test("width property passes") {
    passes("width_property")
  }

  test("border property passes") {
    passes("border_property")
  }
}
