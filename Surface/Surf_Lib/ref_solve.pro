Function Ref_solve, qval = qvl, rval = rvl, serr = ser, zval = zvl, dval = dvl,$
	filter = flt, fit_show = fsh, dens_show = dsh, $
	chi = chi, covmat = cvm, error = err, stat = sta, _extra = _e

;+
; NAME:
;		REF_SOLVE
; VERSION:
;		4.9
; PURPOSE:
;		Calculates a density profile corresponding to a provided relative (to
;		Fresnel) reflectivity curve.
; CATEGORY:
;		Mathematical, x-ray specific.
; CALLING SEQUENCE:
;		Result = REF_SOLVE( QVAL = QVL, RVAL = RVL, SER = SER, ZVAL = ZVL,
;					[, DVAL = DVL] [keywords])
; INPUTS:
;		None.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	QVAL
;		Possibilities:
;
;		1)	Numeric vector.  The Q values at which the reflectivity was
;			measured.
;		2)	A [2,*] array.  In this case the first column is taken as QVAL and
;			the second as RVAL (see below).
;		3)	A [3,*] array.  In this case the first column is taken as QVAL, the
;			second as RVAL and the third as the statistical errors of RVAL.
;	RVAL
;		Numeric vector, a set of relative reflectivity values.  Mandatory if
;		QVAL is given as vector (case (1) above), in which case RVAL has to be
;		of same length, forbidden otherwise.
;	SERR
;		Numeric vector, a list of the stat errors of the reflectivity values.
;		Optional but if given it must be of same length as QVAL.
;
;		Note:	If SERR is given, it overrides any other set of error values
;				which might've been provided in QVAL.
;	ZVAL
;		Possibilities:
;
;		1)	Numeric vector.  The Z (depth) values at which the density changes.
;		2)	A [2,*] array.  In this case the first column is taken as ZVAL and
;			the second as DVAL (see below).
;	DVAL
;		Numeric vector, a set of initial guesses for the density values D
;		corresponding to ZVAL.  The value DVAL[i] is the density in the interval
;		(ZVAL[i-1] - ZVAL[i]).  Optional, but if given must be of same length as
;		ZVAL, forbidden when ZVAL is given as a [2,*] array.
;
;		Note 1:	Both Z[0] and D[0] are expected to be 0.  If Z[0] > 0, zeroes
;				are tagged on to the beginning of Z and D.  If D[0] > 0 (with
;				Z[0] = 0, an error results.
;		Note 2:	If D is not given, it is internally replaced by a vector with
;				first entry of 0 and all other entries of 1.
;	FILTER
;		Integer, if provided and non-zero specifies filtering high frequencies
;		from the result.  The range of values for FILTER is [0,NZ/2] where NZ
;		is the length of Z (and D) and the higher the value, the lower the
;		upper frequency limit is set.  For FILTER = NZ/2 the output is constant
;		i.e. zero frequency only.
;	/FIT_SHOW
;		Switch.  If set, a plot of the input reflectivity values and the fit is
;		produced.
;	/DENS_SHOW
;		Switch.  If set, a plot of the calculated relative density is produced.
;	CHI
;		Optional output, see below.
;	COVMAT
;		Optional output, see below.
;	ERROR
;		Optional output, see below.
;	STATUS
;		Optional output, see below.
;	_EXTRA
;		A formal keyword used to pass keywords to FGH_EXT, as well as
;		plotting keywords.  Not to be used directly.  Keywords of interest are
;
;		TRY	:			Specifes number of iteration tries before giving up.
;						Default is 10.
;		/PROGRESS	:	Switch.  If set, the iteration progress is printed to \
;						the screen.
; OUTPUTS:
;		Returns the density values (same dimension and interpretation as DVAL)
;		providing the best fit for the measured relative reflectivity profile.
; OPTIONAL OUTPUT PARAMETERS:
;	CHI
;		Returns the calculated Chi squared per degree of freedom.
;	COVMAT
;		Returns the full covariance matrix of the return values.
;	ERROR
;		List of the errors of the return values (same length and order).
;	STATUS
;		Scalar output, shows the status of the iteration.  Value of 0 means
;		"did not converge".  If this occurs, can try with higher value of TRY.
; COMMON BLOCKS:
;		None
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		As mentioned in the "notes"., above.
; PROCEDURE:
;		Organizes the data, generates filters as needed, and calls on FGH_EXT,
;		with REF_FUN, to perform the actual optimization, and on DENSPLOT to
;		display results (if required).  Calls BASE_GEN, CALCTYPE, CAST, DEFAULT,
;		DIAGOVEC, DIF, ISNUM, FGH_EXT, MAKE_RANGE, SPLIT_XY, TOLER, TRUCOL and
;		TYPE, from MIDL.
; MODIFICATION HISTORY:
;		Created 1-NOV-2003 by Mati Meron.
;		Streamlined 1-FEB-2004 by Mati Meron.
;		Upgraded 10-FEB-2004.  Added keywords FIT_SHOW, DENS_SHOW and CHI.
;		Modified 25-APR-2004.  Changed from squared to regular errors.
;-

	on_error, 1
	fnam = 'ref_fun'
	stat = 0

	nq = Split_xy(qvl, rvl, x = q, y = r, z = iser, inpx = inf, /keep)
	if nq gt 0 and inf then begin
		typ = Calctype(0.,q,r)
		qeps = nq*Toler(typ=typ)
		aser = abs(Default(ser,Default(iser,replicate(1.,nq))))
		if n_elements(aser) ne nq then message, 'Data err values inconsistency!'
		dum = where(aser eq 0,ndum,comp=cdum,ncomp=ncdum)
		if ndum gt 0 then begin
			if ncdum gt 0 then mser = min(aser[cdum]) < qeps else mser = 1
			aser[dum] = mser
		endif else mser = min(aser) < qeps
		w = 1/aser^2
		s = sort(q)
		q = q[s]
		r = r[s]
		w = w[s]
		if q[0] ge 0 then begin
			if q[0] gt 0 then begin
				q = [0,q]
				r = [1,r]
				w = [1/mser^2,w]
				nq = nq + 1
			endif else if abs(r[0]-1) gt qeps then message, 'R[0] must equal 1!'
		endif else message, 'Q must be nonnegative!'
	endif else message, 'Missing or inconsistent Q and/or reflectivity values!'

	nz = Split_xy(zvl, dvl, x = z, y = d, inpx = ins, /keep)
	if nz gt 0 then begin
		if ins then begin
			zeps = nz*Toler(z)
			s = sort(z)
			z = z[s]
			d = d[s]
		endif else begin
			s = sort(d)
			z = d[s]
			d = replicate(Cast(1.,Type(z)),nz)
			d[0] = 0
		endelse
		zeps = nz*Toler(z)
		if z[0] ge 0 then begin
			if z[0] gt 0 then begin
				z = [0,z]
				d = [0,d]
				nz = nz + 1
			endif else if abs(d[0]) gt zeps then message, 'D[0] must equal 0!'
		endif else message, 'Z must be nonnegative!'
	endif else message, 'Missing or inconsistent Z and/or density values!'

	nh = nz/2
	flt = Default(flt,0,/dtyp)
	if flt gt nh then begin
		message, 'filter value too high, reset to max = ' + string(nh), /con
		flt = nh
	endif
	if flt gt 0 then begin
		fcon = nz - 1 - 2*(nh-flt)
		bas = Base_gen(z,/scale)
		ind = [0,Make_range(nz - [fcon,1])]
		vecc = bas[*,ind]
		valc = [1,replicate(0,fcon)]
	endif else begin
		vecc = replicate(1.,nz)
		valc = 1
	endelse

	ddin = (Dif([d,1.]))[1:*]
	dum = call_function(fnam,ddin,z,q)
	dd = FGH_ext(fnam,x_ini=ddin,/sum,yval=r,weigh=w,/min,$
		vec_con=vecc,val_con=valc,stat=sta,chi=chi,covmat=dcvm,_extra=_e)
	dfit = Cast(([0,total(dd,/cum)])[0:nz-1],typ,typ,/fix)

	cvm = make_array(nz,nz,typ=typ)
	if nz gt 1 then begin
		mat = make_array(nz,nz,typ=typ)
		for i = 0l, nz-1 do for j = 0l, nz-1 do mat[i,j] = i ge j
		dcvm = mat# dcvm# transpose(mat)
		cvm[1:*,1:*] = dcvm[0:nz-2,0:nz-2]
		esq = Diagovec(cvm)
		dum = where(esq lt 0,ndum)
		err = sqrt(abs(esq))
		if ndum gt 0 then err[dum] = (machar(double=Isnum(x,/double))).xmax
	endif else err = Cast(0,typ,typ)

	if keyword_set(dsh) then Densplot, z, dfit
	if keyword_set(fsh) then begin
		window, 1
		tru = Trucol([[4,0,0],[0,4,0]])
		plot, q, r, /nodata, _extra = _e
		oplot, q, r, col = tru[0]
		oplot, q, call_function(fnam,dd), col = tru[1]
		wset, 0
	endif

	return, dfit
end