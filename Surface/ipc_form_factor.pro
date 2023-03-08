Function IPC_form_factor, hq, vq, size= siz, sigma= sig, width= wid, mass=mas,$
	mean = mea, parallel = par, perpendicular = per, approximate = apr, $
	radians = rad, alp = alp, lam = lam, energy = ene, k = kvl, $
	normalize = nrm, pack = pac, progress = prg, _extra =_e

;+
; NAME:
;		IPC_FORM FACTOR
; VERSION:
;		8.481
; PURPOSE:
;		Calculating the in-plane cylinder scattering form factor.
; CATEGORY:
;		Scattering calculations.
; CALLING SEQUENCE:
;		Result = IPC FORM_FACTOR( HQ [, VQ], SIZE = SIZ, [additional keywords] )
; INPUTS:
;	HQ
;		The value(s) of the horizontal (inplane) part of the Q-vector.
;	VQ
;		The value(s) of the vertical (Z) component of the Q-vector.
;
;		Note:	A number of input schemes are available, as follows:
;
;			1)	Only HQ given.  In this case HQ needs to be a [3,N,M] array
;				with M >= 2.  The HQ[0,*,*] part is used as HQ internally
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
;		None.
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
; 		polydisperse distribution is multiplied by their relative masses (i.e.
; 		the square of their diameters).  If WIDTH and SIGMA are not provided,
; 		MASS has no effect.
; 	/MEAN														|
; 		Switch.  Specifies evaluation of a mean form factor	for	|
; 		randomly (horizontally) oriented cylinders.				|	Only one of
; 	/PARALLEL													|	these three
; 		Switch.  Specifies cylinders oriented parallel to the	|	may be set.
; 		incoming K-vector.										|	Default is
; 	/PERPENDICULAR												|	MEAN.
; 		Switch.  Specifies cylinders oriented perpendicular to 	|
; 		the incoming K-vector.									|
; 	/APPROXIMATE
; 		Switch.  If set and MEAN is set, the evaluation of the mean form factor
; 		is performed using an analytical approximation, instead of the default
; 		numeric integration.  Less accurate but much faster.
;	/RADIANS
;		Switch.  If set, the the input ALP (see below) is assumed to be in
;		radians.  Default is degrees.
; 	ALP
; 		The Alpha angle of the incoming K-vector, measured relative to the
; 		horizontal direction.  K is assumed to be |K|*[0,cos(ALP),-sin(ALP)].
; 		Alpha is given in degrees, unless RADIANS is set.
;
; 		Note:	In the default MEAN mode the K-vector is not used.
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
; 		array result (see INPUT_REFORM for details).  When this is the case and
; 		PACK is set, the result is returned as a [4,M,N] array (standard data
; 		format) where the [0,M,N], [1,M,N] and [2,M,N] slices contain the
; 		horizontal Q-values, vertical Q-values and Form Factor results.
; 	PROGRESS
; 		Specifies calculation progress notifications.  Active only for the case
; 		of polydisperse evaluation.
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
;		Straightforward, from definitions.  Calls INPUT_REFORM, IPC_FUN and
;		KQ_ANG, from SURF_LIB.  Calls ARREQ, CALCTYPE, CAST, HOW_MANY, ONE_OF,
;		ROMBERG, SKERN_BNC and TOLER, from MIDL.  In the case of polydisperse
;		evaluation, calls itself recursively.
; MODIFICATION HISTORY:
;		Created 25-JUL-2016 by Mati Meron.
;		Modified 20-AUG-2016 by Mati Meron.  Added keyword PROGRESS.
;		Modified 20-FEB-2017 by Mati Meron.  Replaced DIAMETER and LENGTH
;		keywords with the single SIZE keyword, for uniformity with FORM_FACTOR.
;		Modified 25-FEB-2017 by Mati Meron.  Added the keywords SIGMA, WIDTH and
;		MASS, thus combining IPC_FORM_FACTOR with the previously separate
;		routine POLY_IPC (which is no longer needed).
;		Modified 5-MARCH-2017 by Mati Meron.  Added keyword APPROXIMATE.
;-

	on_error, 1
	eps = Toler()

	if n_elements(siz) eq 2 then hsiz = siz/2. $
	else message, 'Object size must be provided!'

	typ = Calctype(hq,vq,0.,def=4)
	Input_reform, hq, vq, whq=whq, wvq=wvq, type=5, dim=dim

	case How_many(fir=sig,sec=wid) of
		0	:	begin
					whi = One_of(mea,par,per) > 0
					if whi eq 0 then begin
						idim = ([dim,0])[0:1] > 1
						uhq = reform(whq,idim)
						uvq = reform(wvq,idim)
						if not keyword_set(apr) then begin
							res = make_array(idim,typ=5)
							for i = 0, idim[0]-1 do begin
								for j = 0, idim[1]-1 do begin
									res[i,j] = Romberg('ipc_fun',[0,!dpi],eps,$
									/rel,/ike,hq= uhq[i,j],vq= uvq[i,j],$
									radius=hsiz[0],hlen=hsiz[1],$
									/verify,try=2,_extra=_e)
								endfor
							endfor
							res = reform(res/!dpi)
						endif else begin
							qq = sqrt((uhq^2+uvq^2-uhq/(2*hsiz[1])) > 0)
							res = Binc(qq*hsiz[0],1)^2/ (uhq*hsiz(1) > 1)
						endelse
					endif else begin
						if keyword_set(rad) then walp = 1d*alp $
						else walp = !dpi/180d*alp
						qang = KQ_ang( whq,wvq,$
						alp=walp,lam=lam,energy=ene,k=kvl,/rad,/hor)
						psi = qang - (whi-1)*!dpi/2
						res = $
						IPC_fun(psi,hq=whq,vq=wvq,radius=hsiz[0],hlen=hsiz[1])
					endelse
				end
		1	:	message, 'Both SIGMA and WIDTH needed for polydispersity!'
		2	:	begin
					prfl = keyword_set(prg)
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
					if keyword_set(prg) then begin
						print
						it = systime(1)
						prfl = 1
					endif else prfl = 0
					for i = 0, 2*n do begin
						res[*,*,i]= wei[i]*IPC_form_factor(hq,vq,siz=wsiz[*,i],$
						mean = mea, paral = par, perpend = per, approx = apr, $
						radians= rad, alp= alp, lam= lam, energy= ene, k= kvl)
						if prfl then begin
							fform = '("	",i2," out of ",i2," done, "'
							if keyword_set(apr) then sform = $
							',f9.4," sec. elapsed, ",f9.4, " sec. left")' $
							else sform = $
							',f6.1," sec. elapsed, ",f6.1, " sec. left")'
							tt = systime(1)
							et = tt - it
							rt = et*(2*n-i)/(i+1)
							print, i+1, m, et, rt, form = fform + sform
						endif
					endfor
					if prfl then print
					if n gt 0 then res = total(res,3)
				end
	endcase
	if keyword_set(nrm) then res = res/(max(res) > eps)

	if keyword_set(pac) and n_elements(dim) eq 2 then begin
		ores = res
		res = make_array([4,dim],typ=5)
		res[0,*,*] = whq
		res[1,*,*] = wvq
		res[2,*,*] = ores
	endif else if Arreq(dim,0) then res = res[0]

	return, Cast(res,typ,typ,/fix)
end