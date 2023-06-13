
import org.scalatest.funsuite.AnyFunSuite

class TeamSuite extends AnyFunSuite {
  test("test for a small team") {
    val dev1 = Developer("Jack", (Language.Scala,3),None)
    val dev2 = Developer("John", (Language.Go,1),Some(Language.Scala,3))
    val dev3 = Developer("Joe", (Language.Scala,2),None)
    val team = Team(dev1, dev2, dev3)
    val manager = Manager("Joan", 5, team)
    val numEmployees = EmployeeServices.numEmployees(manager)
    val numOfDevsWithName = EmployeeServices.countDevsWithPrefix(manager,"John")
    val DevsWithLang = EmployeeServices.findDevsWithLanguage(manager, Language.Scala)
    val numOfDevsWithLangAndManager = EmployeeServices.countDevsWithLanguageAndExperiencedManager(manager, 4, Language.Go)
    val numOfDevsWithLangAndManager2 = EmployeeServices.countDevsWithLanguageAndExperiencedManager(manager, 5, Language.Go)
    val numOfDevsWithLangAndManager3 = EmployeeServices.countDevsWithLanguageAndExperiencedManager(manager, 4, Language.Scala)
    val nameOfManagers1 = EmployeeServices.namesOfManagersOfExperiencedDeveloper(manager,1)
    val nameOfManagers2 = EmployeeServices.namesOfManagersOfExperiencedDeveloper(manager,3)
    assert(numEmployees === 4)
    assert(numOfDevsWithName === 1)
    assert(DevsWithLang === List[Developer](dev1,dev2,dev3))
    assert(numOfDevsWithLangAndManager === 1)
    assert(numOfDevsWithLangAndManager2 === 0)
    assert(numOfDevsWithLangAndManager3 === 3)
    assert(nameOfManagers1 === List[String]("Joan"))
    assert(nameOfManagers2 === List[String]("Joan"))

  }
  test("test for a medium team") {
    val dev1 = Developer("Jack", (Language.Scala,3),None)
    val dev2 = Developer("John", (Language.Go,1),Some(Language.Scala,3))
    val dev3 = Developer("Joe", (Language.Scala,2),None)
    val team = Team(dev1, dev2, dev3)
    val manager = Manager("Joan", 5, Team(
      dev1, dev2,dev3,
      Manager("James", 7, Team(Developer("Johnson", (Language.Go,1), None),
                                Developer("John", (Language.Scala,2),Some(Language.Lua,2))))
    ))
    val numEmployees = EmployeeServices.numEmployees(manager)
    val numOfDevsWithName = EmployeeServices.countDevsWithPrefix(manager,"John")
    val DevsWithLang = EmployeeServices.findDevsWithLanguage(manager, Language.Scala)
    val DevsWithLangGo = EmployeeServices.findDevsWithLanguage(manager, Language.Go)
    val numOfDevsWithLangAndManager = EmployeeServices.countDevsWithLanguageAndExperiencedManager(manager, 4, Language.Go)
    val numOfDevsWithLangAndManager2 = EmployeeServices.countDevsWithLanguageAndExperiencedManager(manager, 7, Language.Go)
    val numOfDevsWithLangAndManager3 = EmployeeServices.countDevsWithLanguageAndExperiencedManager(manager, 6, Language.Scala)
    val nameOfManagers1 = EmployeeServices.namesOfManagersOfExperiencedDeveloper(manager,1)
    val nameOfManagers2 = EmployeeServices.namesOfManagersOfExperiencedDeveloper(manager,4)
    assert(numEmployees === 7)
    assert(numOfDevsWithName === 3)
    assert(DevsWithLang === List[Developer](dev1,dev2,dev3,Developer("John", (Language.Scala,2),Some(Language.Lua,2))))
    assert(DevsWithLangGo === List[Developer](dev2,Developer("Johnson", (Language.Go,1), None)))
    assert(numOfDevsWithLangAndManager === 2)
    assert(numOfDevsWithLangAndManager2 === 0)
    assert(numOfDevsWithLangAndManager3 === 1)
    assert(nameOfManagers1 === List[String]("Joan","James"))
    assert(nameOfManagers2 === List[String]())
  }

}
