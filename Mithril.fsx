open System

type NobleMetal = Gold | Silver

type JewelKind = Ruby | Emerald | Sapphire | Diamond
type JewelSize = Tiny | Regular | Massive

type Loot = 
 | Chalice of NobleMetal 
 | Coins of int * NobleMetal
 | Arkenstone 
 | Jewel of JewelKind * JewelSize
 | Bag of Loot list

let jewelValue =
  function
    | Ruby -> 100
    | Emerald -> 200
    | Sapphire -> 300
    | Diamond -> 500 

let rec lootValue =
  function
    | Chalice (Gold) -> 500
    | Chalice (Silver) -> 200
    | Coins (n, Gold) -> 3 * n
    | Coins (n, Silver) -> n
    | Bag (loot) -> loot |> List.map lootValue |> List.sum
    | Jewel (kind, size) -> 
      match size with
      | Tiny -> jewelValue kind
      | Regular -> 2 * jewelValue kind
      | Massive -> 3 * jewelValue kind
    | Arkenstone -> 10000000

let rec justGold (l: Loot) = 
  match l with
  | Chalice (Gold) | Coins (_, Gold) -> [l]
  | Bag (loot) -> loot |> List.collect (fun it -> justGold it)
  | _ -> []

let someLoot = 
  Bag [ Chalice (Silver); Chalice (Gold); Arkenstone; Bag [ Chalice (Gold); Coins (5, Silver); Coins (50, Gold) ]]
   
type Race = Hobbit | Dwarf | Elf | Man

type RingResilience = HighResilience | MediumResilience | LowResilience

let resilience r = 
  match r with
    | Hobbit -> HighResilience
    | Man -> LowResilience
    | _ -> MediumResilience

type MagicIntensity = MagicIntensity of int

type Enchantment = Blessing of MagicIntensity | Curse of MagicIntensity

let createMagicIntensity intensity = 
  if intensity > 0 && intensity <= 5 && intensity <> 4 then
    Some (MagicIntensity intensity)
  else 
    None

let createBlessing v = 
  match createMagicIntensity v with
  | Some m -> Some (Blessing m)
  | None -> None

let createCurse v =
  match createMagicIntensity v with
  | Some m -> Some (Curse m)
  | None -> None

type WeaponName = WeaponName of string

type SpecialWeaponFeature =
  | GlowsWhenEnemiesAreNear
  | SpeakingAbility

type WeaponKind = Axe | Club | Sword | Staff | Knife | Bow

type Weapon = 
  | RegularWeapon of WeaponKind 
  | EnchantedWeapon of (WeaponKind * Enchantment)
  | NamedWeapon of (WeaponName * WeaponKind * Enchantment * SpecialWeaponFeature)

let sting = NamedWeapon (WeaponName "Sting", Knife, Blessing (MagicIntensity 3), GlowsWhenEnemiesAreNear)

type RegularArmorMetal = ArmorSteel | ArmorIron
type MagicalArmorMetal = ArmorSilver | Mithril
type RegularArmorKind = ChainMail of RegularArmorMetal | LeatherArmor
type MagicalArmorKind = MagicalChainMail of MagicalArmorMetal
type Armor = RegularArmor of RegularArmorKind | EnchantedArmor of (MagicalArmorKind * Enchantment)

let bilbosArmor = EnchantedArmor (MagicalChainMail Mithril, Blessing (MagicIntensity 3))

type Name = Name of string

type Vocation = Warrior | Wizard | Ranger | Burglar | Gardener

type Level = Level of int

type LimitedValue = { Current: int; Max: int }
type Health = Health of LimitedValue
type Mana = Mana of LimitedValue

let createLimited m = { Current = m; Max = m}
let createHealth h = Health (createLimited h)
let createMana m = Mana (createLimited m)

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
 | Potion of PotionEffect
 | Ring of RingEffect list
 | Treasure of Loot
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

let containsRing items = 
  let ring = items |> List.tryFind (fun x -> x = Ring [Invisibility; Longevity; Corrupting])
  ring.IsSome

let isRingBearer adv = 
  match adv with
  | {
        Inventory = inv
    } when containsRing inv
    -> true
  | _ -> false

let surviveAmonSul adventurer = 
  if isRingBearer adventurer then
    match adventurer with
    | { 
        Race = Hobbit;
        Armor = Some (EnchantedArmor (MagicalChainMail (Mithril), Blessing (MagicIntensity 3)))
      }
      -> true
    | _ -> false
  else true

let theOneRing = Ring [Invisibility; Longevity; Corrupting];

type Fellow = Alive of Adventurer | Sleeping of Adventurer | Dead of Adventurer

let increaseLimitedValue (inc : int) (lv: LimitedValue) = 
  match lv with
    { Current = c; Max = m } 
    ->
    { lv with Current = Math.Max(0, (Math.Min (m, c + inc))) }

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

let frodo = {
    Name = Name "Frodo";
    Race = Hobbit;
    Class = Burglar;
    Level = Level 10;
    Weapon = Some sting; 
    Armor = Some bilbosArmor;
    Health = Health { Current = 20; Max = 20 }
    Mana = None
    Inventory = [theOneRing; Treasure (Coins (123, Gold)) ]
}

let sam = {
    Name = Name "Samwise";
    Race = Hobbit;
    Class = Gardener;
    Level = Level 9;
    Weapon = None;
    Armor = None;
    Health = Health { Current = 25; Max = 25 }
    Mana = None
    Inventory = [ Food (Apples, 10); Treasure (Chalice Silver) ]
}

let gimli = {
    Name = Name "Gimli";
    Race = Dwarf;
    Class = Warrior;
    Level = Level 30;
    Weapon = Some (EnchantedWeapon (Axe, Blessing (MagicIntensity 5)))
    Armor = Some (EnchantedArmor (MagicalChainMail (ArmorSilver), Blessing (MagicIntensity 1)));    
    Health = Health { Current = 80; Max = 80 }
    Mana = None
    Inventory = [ Treasure (Coins (100, Gold)); Treasure (Coins (80, Silver)); Treasure Arkenstone ]
}

let legolas = {
    Name = Name "Legolas";
    Race = Elf;
    Class = Ranger;
    Level = Level 30;
    Weapon = Some (EnchantedWeapon (Bow, Blessing (MagicIntensity 5)))
    Armor = Some (RegularArmor (LeatherArmor))
    Health = Health { Current = 60; Max = 60 }
    Mana = Some (Mana { Current = 10; Max = 10 })
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

let samRingbearer = { sam with Inventory = theOneRing :: sam.Inventory }

let getLootFrom (a : Adventurer) = 
  match a with 
  {
    Inventory = inv
  }
  -> inv |> List.map (fun it -> match it with Treasure t -> Some t | _ -> None)
         |> List.filter (fun it -> Option.isSome it)
         |> List.map (fun it -> it.Value)
