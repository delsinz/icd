with Ada.Numerics;          use Ada.Numerics;
with Ada.Numerics.Float_Random;          use Ada.Numerics.Float_Random;
with Ada.Numerics.Elementary_Functions;  use Ada.Numerics.Elementary_Functions;
with Ada.Text_IO;

package body RandomNumber is

   -- a random number generator
   Seed  : Generator;

   -- Taken from: http://rosettacode.org/wiki/Random_numbers#Ada and
   -- corrected to use ln instead of log base 10
   function NormalFloat(Mu    : Float;
                        Sigma : Float) return Float is
      Result : Float;
   begin
      Reset(Seed);
      Result := Mu + (Sigma * Sqrt (-2.0 * Log (Random (Seed))) *
                        Cos (2.0 * Pi * Random (Seed)));
      return Result;
   end NormalFloat;

   function NormalInteger(Mu : Integer;
                          Sigma : Integer) return Integer is
   begin
      return Integer(NormalFloat(Float(Mu), Float(Sigma)));
   end NormalInteger;

   function UniformFloat(Lower, Upper : Float) return Float is
      Base : Uniformly_Distributed;
      Result : Float;
   begin
      Reset(Seed);
      Base := Random(Seed);
      Result := Lower + (Upper - Lower) * Base;
      return Result;
   end UniformFloat;

   function UniformInteger(Lower, Upper : Integer) return Integer is
   begin
      return Integer(UniformFloat(Float(Lower), Float(Upper)));
   end UniformInteger;

   function UniformFloatWithError(Value : in Float; Error : in Float)
                                 return Float is
      Lower, Upper : Float; -- The lower and upper bounds given the error
      RandomValue : Float;
   begin
      Reset(Seed);
      Lower := Value * (1.0 - Error);
      Upper := Value * (1.0 + Error);
      RandomValue := UniformFloat(Lower, Upper);
      return RandomValue;
   end UniformFloatWithError;

   function UniformIntegerWithError(Value : in Integer; Error : in Float)
                                   return Integer is
   begin
      return Integer(UniformFloatWithError(Float(Value), Error));
   end UniformIntegerWithError;

   function RandomBooleanWithBias(Prob : in Uniformly_Distributed)
                                 return Boolean is
   begin
      return Random(Seed) <= Prob;
   end RandomBooleanWithBias;

   function RandomBooleanUnbiased return Boolean is
   begin
      return RandomBooleanWithBias(0.5);
   end RandomBooleanUnbiased;

end RandomNumber;
