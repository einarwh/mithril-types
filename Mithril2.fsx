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

type SupplyValue = SupplyValue of int
type Supply = { Current: SupplyValue; Max: SupplyValue }
type Health = Health of Supply
type Mana = Mana of Supply

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

let bilbosArmor = EnchantedArmor (MagicChainMail (Mithril), Blessing (MagicIntensity 3))

let frodo = {
    Name = Name "Frodo";
    Race = Hobbit;
    Class = Burglar;
    Level = Level 10;
    Weapon = Some sting; 
    Armor = Some bilbosArmor;
    Health = Health { Current = SupplyValue 20; Max = SupplyValue 20 }
    Mana = None
    Inventory = []
}

let sam = {
    Name = Name "Samwise";
    Race = Hobbit;
    Class = Gardener;
    Level = Level 8;
    Weapon = Some (RegularWeapon Pebbles);
    Armor = None;
    Health = Health { Current = SupplyValue 25; Max = SupplyValue 25 }
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
    Health = Health { Current = SupplyValue 80; Max = SupplyValue 80 }
    Mana = None
    Inventory = []
}

let legolas = {
    Name = Name "Legolas";
    Race = Elf;
    Class = Ranger;
    Level = Level 30;
    Weapon = Some (RegularWeapon Bow)
    Armor = Some (RegularArmor LeatherArmor)
    Health = Health { Current = SupplyValue 60; Max = SupplyValue 60 }
    Mana = Some (Mana { Current = SupplyValue 10; Max = SupplyValue 10 })
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

    