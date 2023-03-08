Function TC_Form_factor, hq, vq, size= siz, sigma= sig, width= wid, mass= mas, $
	radians = rad, alp = alp, tilt = til, orientation = ori, $
	lam = lam, energy = ene, k = kvl, normalize = nrm, pack = pac

;+
; NAME:
;		TC_FORM FACTOR
; VERSION:
;		8.49
; PURPOSE:
;		Calculating scattering form factors for tilted cylinders.
; CATEGORY:
;		Scattering
; CALLING SEQUENCE:
;		Result = TC_FORM_FACTOR( HQ [, VQ], {/SPHERE,/ELLIPSOID,/CYLINDER}, $
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
; 		2-element vector, the cylinder's dimensions in [diameter,length] order.
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
; 		the square of their diameters.  If WIDTH and SIGMA are not provided,
; 		MASS has no effect.
;	/RADIANS
;		Switch.  If set, the the inputs ALP, TILT and ORIENTATION (see below)
;		are assumed to be in radians.  Default is degrees.
; 	ALP
; 		The Alpha angle of the incoming K-vector, measured relative to the
; 		horizontal direction.  K is assumed to be |K|*[0,cos(ALP),-sin(ALP)].
; 		Alpha is given in degrees, unless RADIANS is set.  Default value is 0.
; 	TILT
; 		The tilt angle of the cylinder relative to vertical.  Given in degrees
; 		unless RADIANS is set.  Default value is 0.
; 	ORIENTATION
; 		The planar orientation angle of the tilt, relative to the X axis (note,
; 		the incoming beam is along the Y axis).  Given in degrees unless RADIANS
; 		is set.
;	LAM														| When used,
;		Scalar, the value of the wavelength in Angstrem.	| one and only
;	ENE														| one of these
;		Scalar, the energy value in keV.					| three must be
;	K														| defined.
;		Scalar, the K value in inverse Angstrem.			|
;
;		Note:	In the default MEAN mode the K vector is not used, thus the
;				parameters ALP, LAM, ENE, K, are not needed and, if provided,
;				will be ignored.
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
;		Straightforward, from definitions.  Calls INPUT_REFORM and UQ_ANG from
;		SURF_LIB.  Calls itself recursively if needed.  Calls ARREQ, BINC,
;		CALCTYPE, CAST, DEFAULT, HOW_MANY, SKERN_BNC, SP_BESELJ and TOLER,
;		from MIDL.
; MODIFICATION HISTORY:
;		Created 25-JUN-2017 by Mati Meron, using elements from the FORM_FACTOR
;		and IPC_FORM_FACTOR routines.
;-

	on_error, 1

	if n_elements(siz) eq 2 then hsiz = siz/2. $
	else message, 'Object size must be provided!'

	typ = Calctype(hq,vq,0.,def=4)
	Input_reform, hq, vq, whq=whq, wvq=wvq, type=5, dim=dim

	case How_many(fir=sig,sec=wid) of
		0	:	begin
					if keyword_set(rad) then mult = 1d else mult = !dpi/180d
					walp = mult*alp
					if size(walp,/dim) ne 0 then $
					message, 'ALPHA must be a scalar!'
					wtil = mult*Default(til,0.,/dtyp)
					wori = mult*Default(ori,0.,/dtyp)
					tvec = [sin(wtil)*cos(wori),sin(wtil)*sin(wori),cos(wori)]
					wq = sqrt(whq^2 + wvq^2)
					tang = $
					UQ_ang(whq,wvq,alp=walp,lam=lam,ene=ene,k=kvl,uvec=tvec,/rad)
					uhq = wq*sin(tang)
					uvq = wq*cos(tang)
					harg = hsiz[0]*uhq
					varg = hsiz[1]*uvq
					res = (Binc(harg,1)*SP_beselj(varg,0))^2
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
						wsiz = make_array(2,m,type=typ)
						wsiz[0,*] = siz[0] + k*dx
						wsiz[1,*] = siz[1]
						if keyword_set(mas) then begin
							wei = wei*wsiz[0,*]^2
							wei = wei/total(wei)
						endif
					endif else message, 'width for this case cannot exceed ' + $
						string(2*nmax+1,form='(i0,"!")')
					res = make_array(n_elements(hq),n_elements(vq),m,type=typ)
					for i = 0, 2*n do begin
						res[*,*,i] = wei[i]*TC_Form_factor(hq,vq,siz=wsiz[*,i],$
						rad=rad,alp=alp,til=til,ori=ori,lam=lam,ene=ene,k=kvl)
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