Function Mono_calibrate, true_energy = ten, measured_energy = men, $
	crystal = crs, index = ind, corrected = cor, show = sho, error = err

;+
; NAME:
;		MONO_CALIBRATE
; VERSION:
;		8.4
; PURPOSE:
;		Evaluates angle correction for monochromator calibration.
; CATEGORY:
;		Beamline specific.
; CALLING SEQUENCE:
;		Result = MONO_CALIBRATE( TRUE_ENERGY = TEN, MEASURED_ENERGY = MEN $
;				[, CRYSTAL = CRS], INDEX = IND (,keywords)
; INPUTS:
; 		None.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	TRUE_ENERGY
;		Numeric scalar or vector, true energy values for calibration.
;	MEASURED_ENERGY
;		Numeric, same dimensionality as TRUE_ENERGY, the corresponding measured
;		energy values.
;	CRYSTAL
;		Character scalar translating to a name of a crystal.  Default is 'Si'.
;	INDEX
;		Scalar or vector representing a diffraction index.  If a scalar is
;		entered, it is supplemented with '1,1', thus 3 becomes [3,1,1].
;	/CORRECTED
;		Switch.  If set, it means that the measured values include a refractive
;		correction.  Default is uncorrected.
;	/SHOW
;		Switch.  If set, a table of pre and post correction values is printed to
;		the screen.
;	ERROR
;		Optional output, see below.
; OUTPUTS:
;		Returns the calculated angle correction, i.e. the value which needs to
;		be *added* to the monochromator readings for calibration purposes.  The
;		returned value is in degrees.
; OPTIONAL OUTPUT PARAMETERS:
;	ERROR
;		Returns the estimated statistical error of the output.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward.  Calls BRAGG_ANGLE and MONO_ENERGY.  Calls ARREQ, CAST,
;		CODIMS, DEFAULT, FLTROUND, LINFIT_MM and TABULATE, from MIDL.
; MODIFICATION HISTORY:
;		Created 15-FEB-2015 by Mati Meron.
;-

	on_error, 1

	wcrs = Default(crs,'Si')
	if n_elements(ind) eq 1 then wind = [ind,1,1] else wind = ind
	if not (Arreq(wind,[1,1,1]) or Arreq(wind,[3,1,1])) $
	then message, 'Unacceptable index!'
	unc = 1 - keyword_set(cor)

	okfl = Codims(ten,men,same=sam,ninp=nin)
	if sam and nin eq 2 then begin
		tan = Bragg_angle(ene=ten,crys=wcrs,ind=wind,/cold)
		man = Bragg_angle(ene=men,crys=wcrs,ind=wind,/cold,unc=unc)
		det = tan - man
	endif else message, 'Mising or inconsistent inputs!'
	n = n_elements(det)
	res = Linfit_mm(replicate(1,n),det,ord=0,err=err)
	if n gt 1 then err = Fltround(err,dig=3) else err = !null

	if keyword_set(sho) then begin
		print
		cman = man + res
		Tabulate, Cast(ten,4), Cast(men,4), $
		Mono_energy(cman,crys=wcrs,ind=wind,/cold),$
		Mono_energy(cman,crys=wcrs,ind=wind,/cold,unc=unc),$
		head = ['true ene', 'meas. ene', 'true post_corr ', 'meas. post_corr']
		print
	end

	return, Fltround(res,dig=3)
end