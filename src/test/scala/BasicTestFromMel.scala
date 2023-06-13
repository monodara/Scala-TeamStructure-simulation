
import org.scalatest.funsuite.AnyFunSuite

class BasicSuite extends AnyFunSuite {
  val dev1 = Developer("Ellie", (Language.Scala, 5), Some(Language.Lua, 3))
  val root: Manager = Manager("Cato", 7, Team(dev1))

  test("numEmployees returns 2 for two-employee hierarchy") {
    assert(EmployeeServices.numEmployees(root) === 2)
  }

  test("countDevsWithPrefix correct for two-employee hierarchy") {
    assert(EmployeeServices.countDevsWithPrefix(root, "El") === 1)
  }

  test("findDevsWithLanguage correct for two-employee hierarchy") {
    val devList = EmployeeServices.findDevsWithLanguage(root, Language.Lua)
    assert(devList.length === 1)
    assert(devList.head.name === "Ellie")
  }

  test("countDevsWithLanguageAndExperiencedManager correct for two-employee hierarchy"){
    assert(EmployeeServices.countDevsWithLanguageAndExperiencedManager(root, 6, Language.Scala) === 1)
  }

  test("namesOfManagersOfExperiencedDeveloper correct for two-employee hierarchy") {
    val nameList = EmployeeServices.namesOfManagersOfExperiencedDeveloper(root, 6)
    assert(nameList.length === 1)
    assert(nameList.head === "Cato")
  }

  test("fire correct for three-employee hierarchy") {
    val dev1 = Developer("Ellie", (Language.Scala, 5), Some(Language.Lua, 3))
    val dev2 = Developer("Alex", (Language.Go, 5), Some(Language.Scala, 2))
    val root: Manager = Manager("Cato", 7, Team(dev1, dev2))
    val newRoot = EmployeeServices.fire(root, dev1)
    assert(EmployeeServices.numEmployees(root) === 3) // original tree unchanged
    assert(EmployeeServices.numEmployees(newRoot) === 2) // new tree reduced in size
    assert(newRoot.serves.members(0) == dev2) // dev2 is in the new tree
    assert(newRoot.serves.members(0) eq dev2) // dev2 is shared between the two trees
    // (Passing this final assert is "icing on the cake")
  }
}
