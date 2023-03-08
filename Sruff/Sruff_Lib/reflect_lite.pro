Function Reflect_lite, theta = tet, qz = qzv, energy = ekev, wavelength = lamb,$
	dval = dvl, zval = zvl, thick = tarr, tanph = tph, roughness = roff, $
	s_dval=sdvl, s_tanph=stph, degrees=deg, field=fld, rfnorm=rfn, ret_vals=rtv

;+
; NAME:
;		REFLECT_LITE
; VERSION:
;		8.41
; PURPOSE:
;		Calculates the reflection of X-rays off a flat interface.
; CATEGORY:
;		X-ray calculations.
; CALLING SEQUENCE:
;		Result = REFLECT_LITE ([THETA = TET ... QZ = QZV],
;					[ENERGY = EKEV ... WAVELENGTH = LAMB],[, optional keywords])
; INPUTS:
;		None
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	THETA									|	Either THETA or QZ, but never
;		Angle(s) of incidence (glancing).	| 	both, must be given.
;		In radians, unless /DEGREES is set.	|
;	QZ										|
;		Value(s) of Qz (in inverse Angstrem)|
;
;	ENERGY									|	Either Energy or Wavelength,
;		Photon energy (in keV).				|	but never both, must be given.
;	WAVELENGTH								|
;		Photon wavelength (in Angstrem).	|
;	DVAL
;		An array of one or more electron densities, in electron/Angstrem^3.
;	ZVAL												|
;		An array of Z values marking the location of	|	Either ZVAL or
;		the layer boundaries, in Angstrem.  If given,	|	THICK must be given,
;		must be of same length as DVAL.					|	not both.  In the 
;	THICK												|	case of single 
;		Array of layer thicknesses, in Angstrem.  If 	|	interface, neither
;		given, must be of length one less than DVAL.	|	is needed.
;	TANPH
;		Numeric scalar, or vector of same length as DVAL, the ratio of the 
;		imaginary and real parts of the dielectric function.  Defaults to zero,
;		i.e. no imaginary part (absorption).
;	ROUGHNESS
;		Value of surface roughness, in Angstrem. Default value is 0.  Can be
;		provided as scalar (in which case the value provided applies to all
;		interfaces or a vector of same length as number of interfaces (in which
;		case consecutive roughnesses are applied to consecutive interfaces).
;	S_DVAL
;		Scalar, the electron density (in electron/Angstrem^3) of the superphase.
;		Defaults to zero, i.e. no superphase.
;
;		Note:	The superphase is always a single layer.
;	S_TANPH
;		Same as TANPH, for the superphase.  Scalar.
;	/DEGREES
;		Switch.  Specifies that the angles are given in degrees. Default is
;		radians.
;	/FIELD
;		Switch.  If set, reflected field values are returned, normalized so that
;		the incoming incident field is 1.  Note, field values are complex.
;	/RFNORM
;		Switch.  If set, the returned reflectivity is normalized to the Fresnel
;		reflectivity corresponding to the last electron density in DVAL (see
;		above).  Not active if /FIELD is set.
;	RET_VALS
;		Optional output, see below.
; OUTPUTS:
;		Returns the reflection coefficient for all the angles or qz values 
;		provided, unless /FIELD is set in which case field values are returned.
;		Output form is same as input.
; OPTIONAL OUTPUT PARAMETERS:
;	RET_VALS
;		If THETA is given, returns the corresponding QZ and vice versa.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Multiple interface backwards propagation.  Calls ABS_MM, ARREQ, DEFAULT,
;		DIF, FPU_FIX, ONE_OF and REAL_MM, from MIDL.  Calls itself recursively
;		when needed.
; MODIFICATION HISTORY:
;		Created 15-OCT-2012 by Mati Meron, through "surgery" on REFLECT.
;		Modified 25-FEB-2015 by Mati Meron.  Enabled vector TANPH values.
;-

	on_error, 1
	conv = float(!srcon.conv)
	if keyword_set(deg) then amult = !dtor else amult = 1.

	case One_of(ekev,lamb,val=input) of
		0	:	en = input
		1	:	en = conv/input
		else:	message, 'Missing energy/wavelength input!'
	endcase
	k = 2*!pi*en/conv
	sfac = float(4*!pi*1e10*!srcon.re/k^2)

	case One_of(tet,qzv,val=input) of
		0	:	begin
					sang = sin(amult*input)
					rtv = 2*k*sang
				end
		1	:	begin
					sang = input/(2*k)
					rtv = asin(sang)/amult
				end
		else:	message, 'Missing angle/qz input'
	endcase
	sandim = size(sang,/dim)
	nsan = n_elements(sang)
	nlays = n_elements(dvl)

	whi = One_of(zvl,tarr)
	case whi of
		-1	:	d = [0.,0.]
		0	:	d = [abs(Dif(zvl,/cli)),0.]
		1	:	d = [0.,tarr,0.]
	endcase
	if n_elements(d) lt nlays + 1 then message, 'Wrong # of thicknesses!'
	d = d[0:nlays]
	wtph = abs(Default(tph,0.,/dtyp))
	if n_elements(wtph) eq 1 then wtph = replicate(wtph,nlays) $
	else if n_elements(wtph) ne nlays then message, 'Wrong # of TANPH values!'

	ni = (zi = (ephi = (etphi = make_array(nsan, nlays + 1, type = 6))))
	roffl = keyword_set(roff)
	if roffl then begin
		case n_elements(roff) of
			1			:	wrof = [0.,replicate(roff,nlays)]
			nlays		:	wrof = [0.,roff]
			else		:	message, 'Wrong # of roughnesses'
		endcase
		chi = (psi = ni)
	endif else chi = (psi = make_array(nsan,nlays + 1,val = 1.))

	ni[*,0] = sang
	if n_elements(sdvl) eq 0 then sangsq = sang^2 else $
	sangsq = sang^2 + sfac*sdvl*complex(1,-abs(Default(stph,0.,/dtyp)))
	for i = 1l, nlays do begin
		ni[*,i] = sqrt(sangsq - sfac*dvl[i-1]*complex(1,-wtph[i-1]))
		zi[*,i] = (ni[*,i-1] - ni[*,i])/(ni[*,i-1] + ni[*,i])
		ephi[*,i] = exp(complex(0,k*d[i])*ni[*,i])
		etphi[*,i] = (ephi[*,i])^2
		if roffl then begin
			chi[*,i] = exp(-(2*k*wrof[i]*Real_mm(ni[*,i-1]))^2/2)
			psi[*,i] = exp(-(k*wrof[i]*Real_mm(ni[*,i] - ni[*,i-1]))^2/2)
		endif
	endfor

	res = replicate(complex(0.,0.), nsan)
	for i = nlays, 1, -1 do begin
		dumf = psi[*,i]*etphi[*,i]*res
		dums = zi[*,i]*dumf + 1
		res = chi[*,i]*(dumf + zi[*,i])/dums
	endfor

	if not keyword_set(fld) then begin
		res = Abs_mm(res)^2
		if keyword_set(rfn) then res = res/$
		Reflect_lite(theta=tet,qz=qzv,energy=ekev,wavelength=lamb,$
		dval=dvl[-1],tanph=wtph[-1],s_dval=sdvl,s_tanph=stph,degrees=deg)
	endif
	if not Arreq(sandim,0) then res = reform(res,sandim)

	return, FPU_fix(res)
end