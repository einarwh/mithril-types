open System

type Name = Name of string
   
type Race = Hobbit | Dwarf | Elf | Man

type Vocation = Warrior | Wizard | Ranger | Burglar | Gardener

type Level = Level of int

type MagicIntensity = MagicIntensity of int

let createMagicIntensity intensity = 
  if intensity > 0 && intensity <= 5 && intensity <> 4 then
    Some (MagicIntensity intensity)
  else 
    None

type Enchantment = Blessing of MagicIntensity | Curse of MagicIntensity

type WeaponName = WeaponName of string
type WeaponKind = Axe | Club | Sword | Staff | Dagger | Bow | Pebbles
type SpecialWeaponFeature = Speaks | GlowsWhenEnemiesAreNear

type Weapon = 
 | RegularWeapon of WeaponKind
 | EnchantedWeapon of WeaponKind * Enchantment
 | NamedWeapon of WeaponName * WeaponKind * Enchantment * SpecialWeaponFeature

let sting = NamedWeapon (WeaponName "Sting", Dagger, Blessing (MagicIntensity 3), GlowsWhenEnemiesAreNear)

type MagicArmorMetal = ArmorSilver | Mithril
type RegularArmorMetal = Steel | Iron
type RegularArmorKind = RegularChainMail of RegularArmorMetal | LeatherArmor
type MagicArmorKind = MagicChainMail of MagicArmorMetal
type Armor = RegularArmor of RegularArmorKind | EnchantedArmor of MagicArmorKind * Enchantment

type LimitedValue = { Current: int; Max: int }
type Health = Health of LimitedValue
type Mana = Mana of LimitedValue

let createLimited m = { Current = m; Max = m}
let createHealth h = Health (createLimited h)
let createMana m = Mana (createLimited m)


type NobleMetal = Gold | Silver

type Loot = 
 | Chalice of NobleMetal 
 | Coins of int * NobleMetal
 | Arkenstone 
 | Bag of Loot list

let rec lootValue =
  function
    | Chalice (Gold) -> 500
    | Chalice (Silver) -> 200
    | Coins (n, Gold) -> 3 * n
    | Coins (n, Silver) -> n
    | Bag (loot) -> loot |> List.map lootValue |> List.sum
    | Arkenstone -> 1000000

let rec justGold (l: Loot) : Loot list = 
  match l with
  | Chalice (Gold) | Coins (_, Gold) -> [l]
  | Bag (looot) -> looot |> List.collect (fun it -> justGold it)
  | _ -> []

let rec justGoldWithStructure (l : Loot) : Loot option = 
  match l with
  | Chalice (Gold) | Coins (_, Gold) -> Some l
  | Bag (looot) -> 
    let goldLooot = looot |> List.map (fun it -> justGoldWithStructure it)
                          |> List.filter (fun it -> it.IsSome)
                          |> List.map (fun it -> it.Value)
    match goldLooot with
    | [] -> None
    | _ -> Some (Bag goldLooot)
  | _ -> None

let someLoot = 
  Bag [ Chalice (Silver); Chalice (Gold); Arkenstone; Bag [ Chalice (Gold); Coins (5, Silver); Coins (50, Gold) ]]

type PotionEffect = 
  | HealthEffect of int
  | ManaEffect of int
  | Sleep
  | Death

type AttackBonus = AttackBonus of int

type DefenseBonus = DefenseBonus of int

type RingEffect =
  | Invisibility
  | AttackEffect of AttackBonus
  | DefenseEffect of DefenseBonus 
  | Longevity
  | Corrupting

type FoodKind = Apples | Lembas
 
type Item =
 | Food of FoodKind * int
 | Treasure of Loot
 | Potion of PotionEffect
 | Ring of RingEffect list

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

let theOneRing = Ring [Invisibility; Longevity; Corrupting];

type Fellow = Alive of Adventurer | Sleeping of Adventurer | Dead of Adventurer

let increaseLimitedValue (inc : int) (lv: LimitedValue) = 
  match lv with
    { Current = c; Max = mv } 
    -> { lv with Current = (Math.Min (mv, c + inc)) }

let reduceLimitedValue (dec : int) (lv : LimitedValue) = 
  increaseLimitedValue -dec lv

let increaseHealth (inc : int) (Health lv) = 
    Health (increaseLimitedValue inc lv)

let increaseMana (inc : int) (m: Mana option) = 
  match m with
  | Some (Mana lv) -> Some (Mana (increaseLimitedValue inc lv))
  | None -> None

let quaff (p : PotionEffect) a =
  match p with 
  | HealthEffect sv -> Alive { a with Health = increaseHealth sv a.Health }
  | ManaEffect sv -> Alive { a with Mana = increaseMana sv a.Mana }
  | Sleep -> Sleeping a
  | Death -> Dead a

let quaffIfAlive p f = 
  match f with
  | Alive a -> quaff p a
  | _ -> f

let attack (hp : int) (f : Fellow) : Fellow = 
  match f with
  | Alive a ->
    match a with 
      {
        Health = Health h
      } ->
      let lv = reduceLimitedValue hp h
      let b = { a with Health = Health lv }
      match lv with
      | { Current = 0; Max = _ } -> Dead b
      | _ -> Alive b
  | Sleeping a ->
    match a with 
      {
        Health = Health { Current = _; Max = m }
      } ->
      Dead { a with Health = Health { Current = 0; Max = m }}
  | Dead a -> Dead a
 
let bilbosArmor = EnchantedArmor (MagicChainMail (Mithril), Blessing (MagicIntensity 3))

let frodo = {
    Name = Name "Frodo"
    Race = Hobbit
    Class = Burglar
    Level = Level 10
    Weapon = Some sting 
    Armor = Some bilbosArmor
    Health = createHealth 20
    Mana = None
    Inventory = []
}

let sam = {
    Name = Name "Samwise"
    Race = Hobbit
    Class = Gardener
    Level = Level 8
    Weapon = Some (RegularWeapon Pebbles)
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
    Weapon = Some (RegularWeapon Axe)
    Armor = Some (EnchantedArmor (MagicChainMail (ArmorSilver), Blessing (MagicIntensity 2)))
    Health = createHealth 80
    Mana = None
    Inventory = []
}

let legolas = {
    Name = Name "Legolas"
    Race = Elf
    Class = Ranger
    Level = Level 30
    Weapon = Some (RegularWeapon Bow)
    Armor = Some (RegularArmor LeatherArmor)
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

            