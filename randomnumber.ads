with Ada.Numerics.Float_Random;          use Ada.Numerics.Float_Random;

-- A package for randomly generating numbers.
package RandomNumber is

   -- Generate a random integer from a normal distribution
   -- of mean Mu and standard deviation Sigma
   function NormalInteger(Mu : Integer;
                          Sigma : Integer) return Integer;

   -- Generate a random floating point number from a normal
   --  distribution of mean Mu and standard deviation Sigma
   function NormalFloat(Mu : in Float;
                        Sigma : in Float) return Float;

   -- Generate a random integer from a uniform
   --  distribution between Lower and Upper (inclusive).
   function UniformInteger(Lower : in Integer; Upper : in Integer)
                               return Integer;

   -- Generate a random floating point number from a uniform
   --  distribution between Lower and Upper (inclusive).
   function UniformFloat(Lower : in Float; Upper : in Float)
                               return Float;

   -- Generate a random floating point number within an error margin
   --  of a given floating point value
   function UniformFloatWithError(Value : in Float; Error : in Float)
                                 return Float;

   -- Generate a random integer within an error margin
   --  of a given integer value
   function UniformIntegerWithError(Value : in Integer; Error : in Float)
                                   return Integer;

   -- Generate a random boolean, generating True with probability Prob
   -- i.e. implements a biased random choice
   function RandomBooleanWithBias(Prob : in Uniformly_Distributed)
                                 return Boolean;

   -- Generate a random boolean. Implements a fair random choice
   function RandomBooleanUnbiased return Boolean;

end RandomNumber;
