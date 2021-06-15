package io.circe.schema.validation

import cats.syntax.apply._
import io.circe.{Decoder, Json}
import io.circe.schema.{Resolver, Schema}
import java.io.File
import java.net.URI
import munit.FunSuite

case class SchemaTestCase(description: String, data: Json, valid: Boolean)
case class SchemaTest(description: String, schema: Schema[URI], tests: List[SchemaTestCase]) {}

object SchemaTestCase {
  implicit val decodeSchemaTestCase: Decoder[SchemaTestCase] = io.circe.generic.semiauto.deriveDecoder
}

object SchemaTest {
  implicit val decodeSchemaTest: Decoder[SchemaTest] = (
    Decoder[String].at("description"),
    // We have to reset the cursor because the references in the tests are actually wrong.
    Decoder[Schema[URI]].prepare { c =>
      val cs = c.downField("schema")
      cs.success.fold(cs)(_.value.hcursor)
    },
    Decoder[List[SchemaTestCase]].at("tests")
  ).mapN(SchemaTest(_, _, _))
}

abstract class TestSuiteTests(path: String, config: Configuration = Configuration.default) extends FunSuite {
  val compiler: Compiler = Compiler(config)

  val tests: List[SchemaTest] = io.circe.jawn.decodeFile[List[SchemaTest]](new File(path)) match {
    case Right(value) => value
    case Left(error)  => throw error
  }

  val deduplicated = tests
    .foldLeft((Set.empty[String], List.empty[SchemaTest])) { case ((seen, res), c @ SchemaTest(description, _, _)) =>
      (seen + description, if (seen(description)) res else c :: res)
    }
    ._2
    .reverse

  deduplicated.foreach { case SchemaTest(description, schema, tests) =>
    tests.foreach { case SchemaTestCase(caseDescription, data, valid) =>
      val expected = if (valid) "validate successfully" else "fail to validate"
      test(s"$description: $caseDescription should $expected") {
        val Right(resolvedSchema) = Resolver.local(schema).runA(Map.empty)
        val errors = compiler(resolvedSchema.value)(data.hcursor)

        if (valid) {
          assert(errors.isEmpty)
        } else {
          assert(errors.nonEmpty)
        }
      }
    }
  }
}

class AdditionalItemsTestSuiteTests extends TestSuiteTests("tests/tests/draft7/additionalItems.json")
class AdditionalPropertiesTestSuiteTests extends TestSuiteTests("tests/tests/draft7/additionalProperties.json")
class AllOfTestSuiteTests extends TestSuiteTests("tests/tests/draft7/allOf.json")
class AnyOfTestSuiteTests extends TestSuiteTests("tests/tests/draft7/anyOf.json")
class BooleanSchemaTestSuiteTests extends TestSuiteTests("tests/tests/draft7/boolean_schema.json")
class ConstTestSuiteTests extends TestSuiteTests("tests/tests/draft7/const.json")
class ContainsTestSuiteTests extends TestSuiteTests("tests/tests/draft7/contains.json")
class DefaultTestSuiteTests extends TestSuiteTests("tests/tests/draft7/default.json")
//class DefinitionsTestSuiteTests extends TestSuiteTests("tests/tests/draft7/definitions.json")
class DependenciesTestSuiteTests extends TestSuiteTests("tests/tests/draft7/dependencies.json")
class EnumTestSuiteTests extends TestSuiteTests("tests/tests/draft7/enum.json")
class ExclusiveMaximumTestSuiteTests extends TestSuiteTests("tests/tests/draft7/exclusiveMaximum.json")
class ExclusiveMinimumTestSuiteTests extends TestSuiteTests("tests/tests/draft7/exclusiveMinimum.json")
class FormatTestSuiteTests extends TestSuiteTests("tests/tests/draft7/format.json")
class IfThenElseTestSuiteTests extends TestSuiteTests("tests/tests/draft7/if-then-else.json")
class InfiniteLoopDetectionTestSuiteTests extends TestSuiteTests("tests/tests/draft7/infinite-loop-detection.json")
class ItemsTestSuiteTests extends TestSuiteTests("tests/tests/draft7/items.json")
class MaximumTestSuiteTests extends TestSuiteTests("tests/tests/draft7/maximum.json")
class MaxItemsTestSuiteTests extends TestSuiteTests("tests/tests/draft7/maxItems.json")
class MaxLengthTestSuiteTests extends TestSuiteTests("tests/tests/draft7/maxLength.json")
class MaxPropertiesTestSuiteTests extends TestSuiteTests("tests/tests/draft7/maxProperties.json")
class MinimumTestSuiteTests extends TestSuiteTests("tests/tests/draft7/minimum.json")
class MinItemsTestSuiteTests extends TestSuiteTests("tests/tests/draft7/minItems.json")
class MinLengthTestSuiteTests extends TestSuiteTests("tests/tests/draft7/minLength.json")
class MinPropertiesTestSuiteTests extends TestSuiteTests("tests/tests/draft7/minProperties.json")
class MultipleOfTestSuiteTests extends TestSuiteTests("tests/tests/draft7/multipleOf.json")
class NotTestSuiteTests extends TestSuiteTests("tests/tests/draft7/not.json")
class OneOfTestSuiteTests extends TestSuiteTests("tests/tests/draft7/oneOf.json")
class PatternTestSuiteTests extends TestSuiteTests("tests/tests/draft7/pattern.json")
class PatternPropertiesTestSuiteTests extends TestSuiteTests("tests/tests/draft7/patternProperties.json")
class PropertiesTestSuiteTests extends TestSuiteTests("tests/tests/draft7/properties.json")
class PropertyNamesTestSuiteTests extends TestSuiteTests("tests/tests/draft7/propertyNames.json")
// Not currently running remote tests.
//class RefTestSuiteTests extends TestSuiteTests("tests/tests/draft7/ref.json")
//class RefRemoteTestSuiteTests extends TestSuiteTests("tests/tests/draft7/refRemote.json")
class RequiredTestSuiteTests extends TestSuiteTests("tests/tests/draft7/required.json")
class TypeTestSuiteTests extends TestSuiteTests("tests/tests/draft7/type.json")
class UniqueItemsTestSuiteTests extends TestSuiteTests("tests/tests/draft7/uniqueItems.json")

class BignumTestSuiteTests extends TestSuiteTests("tests/tests/draft7/optional/bignum.json")
class ContentTestSuiteTests extends TestSuiteTests("tests/tests/draft7/optional/content.json")
//class EcmascriptRegexTestSuiteTests extends TestSuiteTests("tests/tests/draft7/optional/ecmascript-regex.json")")
class NonBmpRegexTestSuiteTests extends TestSuiteTests("tests/tests/draft7/optional/non-bmp-regex.json")
class FloatOverflowTestSuiteTests
    extends TestSuiteTests("tests/tests/draft7/optional/float-overflow.json", Configuration(true))

class FormatDateTestSuiteTests extends TestSuiteTests("tests/tests/draft7/optional/format/date.json")
class FormatDateTimeTestSuiteTests extends TestSuiteTests("tests/tests/draft7/optional/format/date-time.json")
class FormatEmailTestSuiteTests extends TestSuiteTests("tests/tests/draft7/optional/format/email.json")
class FormatTimeTestSuiteTests extends TestSuiteTests("tests/tests/draft7/optional/format/time.json")
//class FormatUriTestSuiteTests extends TestSuiteTests("tests/tests/draft7/optional/format/uri.json")
//class FormatIdnEmailTestSuiteTests extends TestSuiteTests("tests/tests/draft2019-09/optional/format/idn-email.json")
