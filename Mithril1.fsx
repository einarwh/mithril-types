type Name = Name of string
   
type Race = Hobbit | Dwarf | Elf | Man

type Vocation = Warrior | Wizard | Ranger | Burglar | Gardener

type Level = Level of int

type Weapon = Axe | Club | Sword | Staff | Knife | Bow | Pebbles

type Armor = ChainMail | LeatherArmor 

type LimitedValue = { Current: int; Max: int }
type Health = Health of LimitedValue
type Mana = Mana of LimitedValue

let createLimited m = { Current = m; Max = m}
let createHealth h = Health (createLimited h)
let createMana m = Mana (createLimited m)

type FoodKind = Apples | Lembas
 
type Item =
 | Food of FoodKind * int

type Adventurer = {
   Name: Name;
   Race: Race;
   Class: Vocation
   Level: Level
   
   Weapon: Weapon option
   Armor: Armor option

   Health: Health
   Mana: Mana option

   Inventory: Item list
}

let frodo = {
    Name = Name "Frodo"
    Race = Hobbit
    Class = Burglar
    Level = Level 10
    Weapon = Some Knife 
    Armor = Some ChainMail
    Health = createHealth 20
    Mana = None
    Inventory = []
}

let sam = {
    Name = Name "Samwise"
    Race = Hobbit
    Class = Gardener
    Level = Level 8
    Weapon = Some Pebbles
    Armor = None
    Health = createHealth 25
    Mana = None
    Inventory = [ Food (Apples, 10) ]
}

let gimli = {
    Name = Name "Gimli"
    Race = Dwarf
    Class = Warrior
    Level = Level 30
    Weapon = Some Axe
    Armor = Some ChainMail
    Health = createHealth 80
    Mana = None
    Inventory = []
}

let legolas = {
    Name = Name "Legolas"
    Race = Elf
    Class = Ranger
    Level = Level 30
    Weapon = Some Bow
    Armor = Some LeatherArmor
    Health = createHealth 60
    Mana = Some (createMana 10)
    Inventory = [ Food (Lembas, 20) ]
}

let fellows = [ frodo; gimli; sam; legolas ]

let isRace r (a : Adventurer) = 
  match a with
  | {
        Race = x
    } when x = r
    -> true
  | _ -> false

let rec countRace r f =
  match f with
  | [] -> 0
  | h::t ->
    if isRace r h 
    then 1 + countRace r t
    else countRace r t

