# tensor-algebra
Some frequently required methods for the implementation of problems in the field of continuum mechanics, e.g. tensor algebra. These methods are usually needed and used to implement user subroutines in finite element software written in Fortran. Therefore, this repository provides some elegant interfaces to write your equations as you are used to from Matlab or Python. I've also made some speed improvements (hopefully ;)).

## Modules
Most of the listed functions have been implemented for different input parameter types and are overloaded by the use of `INTERFACE`. The compiler takes care of choosing the appropriate subroutine based on the type of the actual input parameter. Therefore, the readability of your code is increased.

I only implemented the input parameter types and orders I needed, so feel free to extend it.

### [comMath.f](comMath.f) - commonly used math modules
- `round()` >> rounds given number, vector, matrics to given decimal digits
- `unit2()` >> returns an 2nd order unit tensor of given dimension
- `unit4()` >> returns the 4th order symetric unit tensor of given dimension
- `unit4_sym()` >> returns the 4th order symetric unit tensor of given dimension
- `kronckerdelta()` >> returns the kronecker delta
- `levicivita()` >> returns the Levi-Civita-Symbol (permutation symbol)
- `tr()` >> returns the trace of a matrics
- `extract()` >> extracts a vector, matrics (Voigt notation)
- `contract()` >> contracts a vector, matrics (Voigt notation)
- `spec()` >> spectral representation with eigenvalues and eigenvectors
- `in1()` >> returns the first inner product
- `in2()` >> returns the second inner product
- `dyad()` >> returns the dyadic product (outer product)
- `inv()` >> matrix/tensor inversion

### [comConti.f](comConti.f) - common used continuum mechanic modules
- `push_stress_2pk2sig()` >> push-forward of 2nd-order stress tensor (2.Piola-Kirchhoff -> Cauchy); sig_ij = 1/J * F_ik * S_kl * F_jl)
- `push_tang_2pk2sig()` >> push-forward of 4th-order material tensor t4f_ref (material to spacial); res_ijkl = 1/J * F_im * F_jn * F_ko * F_lp * t4f_ref(m,n,o,p)

### [comSub.f](comSub.f) - commonly used sub routines (helpers)
- `countequal()` >> Compares all values in a given vector with each other and returns the amaount of matches
- `countvalue()` >> Counts the given values in a vector
- `numb2str()` >> Casts/Converts an number (int or double) to string
- `pp()` >> Pretty Prints different kind of variable types (e.g. for debugging)

## Example

```fortran
! some of the variable declarations
...
real(8)  :: f_tmp
real(8)  :: t2f_tmp (3,3)
real(8)  :: t4f_tmp (3,3,3,3)
...
real(8)  :: t2f_I (3,3)
real(8)  :: t2f_eps (3,3)  ! log. strain epsilon
real(8)  :: t2f_epsD (3,3)  ! log. viscous strain epsilon
...
real(8)  :: t2f_sigEl (3,3)  ! log. elastic stress
real(8)  :: t4f_tanEl (3,3,3,3)  ! log. elastic tangent
...
t2f_I = unit2(3)

! (ST.VENANT [elastic stress tensor])
! sigEl = lambda * (eps^n+1 : I) * I + 2 * µ * eps^n+1
! tmp = eps^n+1 : I
call in2(f_tmp, t2f_eps, t2f_I, 3)

! sigEl = lambda * tmp * I + 2 * µ * eps^n+1
t2f_sigEl = fMAT_LAME_LAMBDA * f_tmp * t2f_I + 2.0d0 * fMAT_LAME_MY * t2f_eps
            
! (ST.VENANT [lagrangian elasticity tensor])
! tanEl = lambda * I.dyad.I + 2 * µ * I4
! tmp = I.dyad.I
call dyad(t4f_tmp, t2f_I, t2f_I, 3)
! tanEl = lambda * tmp + 2 * µ * I4
t4f_tanEl = fMAT_LAME_LAMBDA * t4f_tmp + 2 * fMAT_LAME_MY * unit4(3)

! print result
pp(t4f_tanEl)
```

## License
This project is licensed under the Beerware License - see the [LICENSE](LICENSE) file for details.
