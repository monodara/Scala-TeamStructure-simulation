/**
 * @author Yuanyuan Lu
 * @create on 2022-11-15-9:17
 */
object EmployeeServices:
  def numEmployees(root: Manager): Int =
    val team = root.serves
    def accumuEmp(employee: Employee): Int =
      employee match
        case d: Developer => 1
        case Manager(_,_,serves) => serves.members.foldLeft(1)((sum,member) => sum + accumuEmp(member))
    team.members.foldLeft(1)((sum,member) => sum + accumuEmp(member))

  def countDevsWithPrefix(root: Manager, prefix: String): Int =
    val team = root.serves
    def countDevsHelper(employee: Employee, prefix: String): Int =
      employee match
        case Developer(nameOfDev,_,_) =>
          if nameOfDev.startsWith(prefix) then 1 else 0
        case Manager(_,_,serves) => serves.members.foldLeft(0)((sum,member) => sum + countDevsHelper(member,prefix))
    team.members.foldLeft(0)((sum,member) => sum + countDevsHelper(member,prefix))

  def findDevsWithLanguage(root: Manager, language: Language): List[Developer] =
    val team = root.serves
    def findDevsHelper(employee: Employee, language: Language, listSoFar: List[Developer]): List[Developer] =
      employee match
        case Developer(_,firstLanguage,secondLanguage) =>
          if firstLanguage._1.equals(language) then listSoFar.appended(employee.asInstanceOf[Developer]) else
            secondLanguage match
              case None => listSoFar
              case Some(secondLang) =>
                if secondLang._1.equals(language) then listSoFar.appended(employee.asInstanceOf[Developer])
                else listSoFar
        case Manager(_,_,serves) => serves.members.map(member => findDevsHelper(member,language, listSoFar)).toList.flatten
    team.members.map(member => findDevsHelper(member,language,List[Developer]())).toList.flatten


  def countDevsWithLanguageAndExperiencedManager(root: Manager,
                                                 noOfYears: Int, language: Language): Int =
    def countDevsWithLanguage(employee: Employee, language: Language): Int =
      employee match
        case Developer(_,firstLang,secondLang) =>
          if firstLang._1.equals(language) then 1
          else
            secondLang match
              case None => 0
              case Some(secondLang) =>
                if secondLang._1.equals(language) then 1 else 0
        case Manager(_,_,serves) => serves.members.map(member => countDevsWithLanguage(member,language)).sum
    val team = root.serves
    if root.yearsOfExperience > noOfYears then
      team.members.map(member => countDevsWithLanguage(member,language)).sum
    else
      team.members.filter(member => member.isInstanceOf[Manager])
        .map(member => countDevsWithLanguageAndExperiencedManager(member.asInstanceOf[Manager],noOfYears,language))
        .sum
  def namesOfManagersOfExperiencedDeveloper(root: Manager, noOfYears: Int): List[String] =
    val team = root.serves
    def checkEmployeesExperience(employee: Employee, noOfYears: Int): Boolean =
      employee match
        case Developer(_,firstLanguage,secondLanguage) =>
          if firstLanguage._2 > noOfYears then true
          else
            secondLanguage match
              case None => false
              case Some(secondLang) =>
                if secondLang._2 + firstLanguage._2 > noOfYears then true else false
        case Manager(_,_,serves) =>
          serves.members.foldLeft(false)(
            (enoughExperienceOrNot,member) => enoughExperienceOrNot || checkEmployeesExperience(member, noOfYears))
    def namesOfManagersHelper(manager: Manager, noOfYears: Int, namesSoFar: List[String]): List[String] =
      val listOfRoot =
        if checkEmployeesExperience(manager,noOfYears) then namesSoFar.appended(manager.name) else namesSoFar
      val team = manager.serves
      val listOfSubManagers = team.members.filter(member => member.isInstanceOf[Manager])
        .map(member => namesOfManagersHelper(member.asInstanceOf[Manager],noOfYears,namesSoFar)).toList.flatten
      listOfRoot ++ listOfSubManagers
    namesOfManagersHelper(root, noOfYears, List[String]())

  def fire(root: Manager, emp: Employee): Manager =
    if root == emp then root
    else
      val team = root.serves
      def compareEmp(manager: Manager, employee: Employee): Manager =
        val name = manager.name
        val yearsOfExperience = manager.yearsOfExperience
        val teamMembersAfterFire = manager.serves.members.filter(_ != emp).toList
        employee match
          case d: Developer =>
            if emp == employee then
              Manager(name, yearsOfExperience, Team(teamMembersAfterFire:_*))
            else manager
          case m: Manager =>
            if emp == employee then
              val subTeamMembers = m.serves.members
              val newTeamMembers = teamMembersAfterFire ++ subTeamMembers.toList
              Manager(name, yearsOfExperience, Team(newTeamMembers:_*))
            else
              m.serves.members.map(compareEmp(m,_)).reduce((x,y) =>
                if x == y then x else
                  if x == m then y else x
              )
      team.members.map(compareEmp(root,_)).reduce((x,y) =>
        if x == y then x else
          if x == root then y else x
      )


abstract class Employee
case class Manager(name: String, yearsOfExperience: Int, serves: Team) extends Employee
case class Developer(name: String, firstLanguage: (Language, Int), secondLanguage: Option[(Language, Int)]) extends Employee

case class Team(members: Employee*)

enum Language:
  case Go, Lua, Scala

