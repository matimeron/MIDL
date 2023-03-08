Function Form_factor, hq, vq, size = siz, sigma= sig, width= wid, mass= mas, $ 
	sphere = sph, ellipsoid = ell, cylinder = cyl, normalize= nrm, pack= pac

;+
; NAME:
;		FORM FACTOR
; VERSION:
;		8.48
; PURPOSE:
;		Calculating scattering form factors.
; CATEGORY:
;		Scattering
; CALLING SEQUENCE:
;		Result = FORM_FACTOR( HQ [, VQ], {/SPHERE,/ELLIPSOID,/CYLINDER}, $
;		SIZE = SIZ [additional keywords] )
; INPUTS:
;	HQ
;		The value(s) of the horizontal (inplane) part of the Q-vector.
;	VQ
;		The value(s) of the vertical (Z) component of the Q-vector.
;
;		Note:	A number of input schemes are available, as follows:
;
;			1)	Only HQ given.  In this case HQ needs to be a [K,N,M] array
;				with K >= 2.  The HQ[0,*,*] part is used as HQ internally
;				while HQ[1,*,*] is used as VQ.
;			2)	Both inputs provided, as scalars.  Used as is.
;			3)	HQ provided as vector and VQ as scalar, or vice versa.  The
;				scalar input will be extended to a vector of same length as the
;				other input, with all components equal to the scalar value.
;			4)	HQ a vector of length M, VQ a vector of length N.  Both vectors
;				will be extended internally to an array of dimensions [M,N].
;			5)	HQ and VQ provided as 2D arrays.  Both arrays have to have same
;				dimensions.
;
;			Any other combination is unacceptable.
; OPTIONAL INPUT PARAMETERS:
; 		None.
; KEYWORD PARAMETERS:
; 	SIZE
; 		Specifies the dimension(s) of the scatterer, as follows:
; 			Sphere:		Scalar, the diameter.
; 			Ellipsoid:	2-element vector, horizontal and vertical diameters,
; 						in this order.
; 			Cylinder:	2-element vector, diameter and length, in this order.
; 	SIGMA
; 		The sigma width of the diameter (first parameter of size) distribution.
; 		Providing SIGMA value signals to the routine that a polydisperse
; 		evaluation is to be used.
; 	WIDTH
; 		Integer, the width of the binary coefficient distribution used for the
; 		polydisperse evaluation.  Odd value, if provided as even, is replaced by
; 		the nearest highest odd value.  Cannot exceed (DIAMETER/SIGMA)^2.
;
; 		Note:	Both SIGMA and WIDTH need to be provided for polydisperse
; 				evaluation.  Providing only one of them yields error.
; 	/MASS
; 		Switch.  If set, the contribution of the various fractions in
; 		polydisperse distribution is multiplied by their relative masses, i.e.
; 		the square of their diameters (cubes, for spherical scatterers).  If
; 		WIDTH and SIGMA are not provided, MASS has no effect.
; 	/SPHERE														|
; 		Switch.  Specifies spherical scatterers.				|	One and
; 	/ELLIPSOID													|	only one of
; 		Switch.  Specifies ellipsoidal scatterers, with one axis|	these three
; 		vertical and the other two (horizontal) being equal.	|	must be set.
; 	/CYLINDER													|	No Defaults.
; 		Switch.  Specifies cylindrical scatterers, with the 	|
; 		cylinder axis in the vertical direction.				|
; 	/NORMALIZE
; 		Switch.  If set the result is normalized so the maximal value is 1.
; 	/PACK
; 		Switch.  Active only when the input is provided in a form yielding 2D
; 		array result (see routine INPUT_REFORM for details).  When this is the
; 		case and PACK is set, the result is returned in the standard data format
; 		of a [4,M,N] array where the [0,M,N], [1,M,N] and [2,M,N] slices contain
; 		the horizontal Q-values, vertical Q-values and Form Factor results.
; OUTPUTS:
;		Returns the calculated form vactor values for the case at hand, in the
;		format of the input (if the input includes a scalar and an array, then
;		in the array format), except when /PACK is set (for this, see above)
; OPTIONAL OUTPUT PARAMETERS:
; 		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward, from definitions.  Calls INPUT_REFORM from SURF_LIB.
;		Calls ARREQ, BINC, CALCTYPE, CAST, HOW_MANY, ONE_OF, SKERN_BNC,
;		SP_BESELJ and TOLER, from MIDL.
; MODIFICATION HISTORY:
;		Created 5-MAY-2016 by Mati Meron.
;		Modified 30-JUL-2016 by Mati Meron.  Internal changes.
;		Modified 25-FEB-2017 by Mati Meron.  Added the keywords SIGMA, WIDTH and
;		MASS, enabling polydisperse evaluation.
;-

	on_error, 1

	nsiz = n_elements(siz)
	if nsiz gt 0 then hsiz= siz/2. else message, 'Object size must be provided!'

	typ = Calctype(hq,vq,0.,def=4)
	Input_reform, hq, vq, whq=whq, wvq=wvq, type=5, dim=dim

	case How_many(fir=sig,sec=wid) of
		0	:	begin
					case One_of(sph,ell,cyl) of
						0	:	begin
									if nsiz ne 1 then message, $
									'Sphere requires single dimension!'
									arg = sqrt(hsiz[0]^2*(whq^2 + wvq^2))
									res = 0*arg + 1
									dum = where(arg ge 5d-5,ndum)
									if ndum gt 0 then res[dum] = $
									(3*SP_beselj(arg[dum],1)/arg[dum])^2
								end
						1	:	begin
									if nsiz ne 2 then message, $
									'Ellipsoid requires two dimensions!'
									arg= sqrt((hsiz[0]*whq)^2 + (hsiz[1]*wvq)^2)
									res = 0*arg + 1
									dum = where(arg ge 5d-5,ndum)
									if ndum gt 0 then res[dum] = $
									(3*SP_beselj(arg[dum],1)/arg[dum])^2
								end
						2	:	begin
									if nsiz ne 2 then message, $
									'Cylinder requires two dimensions!'
									harg = hsiz[0]*whq
									varg = hsiz[1]*wvq
									res = (Binc(harg,1)*SP_beselj(varg,0))^2
								end
						else:	message, 'Shape needs to be defined!'
					endcase
				end
		1	:	message, 'Both SIGMA and WIDTH needed for polydispersity!'
		2	:	begin
					nosph = 1 - keyword_set(sph)
					n = round(wid)/2
					m = 2*n + 1
					k = lindgen(m) - n
					wei = Skern_bnc(n)
					if n gt 0 then dx = sig*sqrt(2./n) else dx = 0.
					nmax = floor(2*(hsiz[0]/sig)^2)
					if n le nmax then begin
						wsiz = make_array(1+nosph,m,type=typ)
						wsiz[0,*] = siz[0] + k*dx
						if nosph then wsiz[1,*] = siz[1]
						if keyword_set(mas) then begin
							wei = wei*wsiz[0,*]^(3-nosph)
							wei = wei/total(wei)
						endif
					endif else message, 'width for this case cannot exceed ' + $
						string(2*nmax+1,form='(i0,"!")')
					res = make_array(n_elements(hq),n_elements(vq),m,type=typ)
					for i = 0, 2*n do begin
						res[*,*,i] = wei[i]*Form_factor(hq,vq,siz=wsiz[*,i],$
						sphere=sph, ellipsoid=ell, cylinder=cyl)
					endfor
					if n gt 0 then res = total(res,3)
				end
	endcase
	if keyword_set(nrm) then res = res/(max(res) > Toler())

	if keyword_set(pac) and n_elements(dim) eq 2 then begin
		ores = res
		res = make_array([4,dim],typ=5)
		res[0,*,*] = whq
		res[1,*,*] = wvq
		res[2,*,*] = ores
	endif else if Arreq(dim,0) then res = res[0]

	return, Cast(res,typ,typ,/fix)
end