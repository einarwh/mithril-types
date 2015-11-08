using System;
using System.Collections.Generic;

namespace Heavy
{
  public enum Vocation {
    Warrior,
    Ranger, 
    Wizard,
    Thief
  }

  public class Adventurer
  {
    public string Name { get; private set; }
    public string Race { get; private set; }
    public Vocation Vocation { get; private set; }
    public int Level { get; private set; }

    public bool IsDead { get; private set; }
    public bool IsMortal { get; private set; }
    
    public string Weapon { get; private set; }
    public int? MagicWeaponModifier { get; private set; }
    public bool WeaponLightsUpWhenEnemiesAreNear { get; private set; }

    public string Armor { get; private set; }
    public bool IsLeatherArmor { get; private set; }
    public int MagicArmorModifier { get; private set; }

    public int CurrentHealth { get; private set; }
    public int MaxHealth { get; private set; }

    public int CurrentMana { get; private set; }
    public int MaxMana { get; private set; }

    public bool IsRingBearer { get; private set; }

    public IEnumerable<IItem> Inventory { get; private set; } 
  }

  public interface IItem {}

  public class Treasure : IItem {}
  public class Food : IItem {}
  public class Ring : IItem {}
}

