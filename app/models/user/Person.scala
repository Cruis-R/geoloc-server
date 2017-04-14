package models.user

/** N'importe quel utilisateur du système */
case class Person(
    firstName: String, lastName: String,
    email: EMail, telephone: TelephoneNumber,
    adress: Adress) {}
