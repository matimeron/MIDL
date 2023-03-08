Function Scan_what, snum, valid = val, strict = str, vpd = vpd, $
	list = lis, status = sta, pd_status = pds, details = det, motors = mot, $
	data_points = dtp, alpha = alp, beta = bet, dth = dth, count = cnt, $ 
	show = sho, ashow = sha, _extra = _e

;+
; NAME:
;		SCAN_WHAT
; VERSION:
;		8.34
; PURPOSE:
;		Returns type information about a list of scans from a SPEC file.
; CATEGORY:
;		SPEC file processing
; CALLING SEQUENCE:
;		Result = SCAN_WHAT ( SNUM [, keywords])
; INPUTS:
;	SNUM
;		Scan list.  Either an array of integers or a character scalar or array
;		of any form acceptable by SCAN_LIST_VER.  If not given or if the single
;		value 0 is provided, it translates to "all scans within the file".
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	/VALID												|
;		Switch.  If set, only valid scans, i.e. scans	|	If both keywords
;		with an odd STATUS value, are recognized.		|	are used in a call,
;	/STRICT												|	STRICT takes
;		Switch.  Same as VALID but stricter, only scans	|	preference.
;		with STATUS = 1 are recognized. 				|
;	/VPD
;		Switch.  If set and if if VALID and/or STRICT are set, only valid/strict
;		area detector scans are recognized.
;	LIST
;		Optional output, see below.
;	STATUS
;		Optional output, see below.
;	PD_STATUS
;		Optional output, see below.
;	DETAILS
;		Optional output, see below.
;	MOTORS
;		Optional output, see below.
;	DATA_POINTS
;		Optional output, see below.
;	ALPHA
;		Optional output, see below.
;	BETA
;		Optional output, see below.
;	DTH
;		Optional output, see below.
;	COUNT
;		Optional output, see below.
;	/SHOW													|
;		Switch.  If set, a list of scan types and motors is	|	At most one of
;		printed to the screen.								|	these two can
;	/ASHOW												|	be used.
;		Switch.  Same as SHOW, only the angles ALPHA, BETA 	|
;		and DTH are printed instead of motors.				|
;	_EXTRA
;		A formal keyword used to pass keywords to SCAN_FIELD READ.  Not to be
;		used directly.
; OUTPUTS:
;		Returns a list of scan types for all the scans on the list SNUM.  The
;		list is a string vector of length NSCAN (where NSCAN is the number of
;		scans)..
; OPTIONAL OUTPUT PARAMETERS:
;	LIST
;		Returns the expanded list of all the scan numbers, as provided by SNUM.
;	STATUS
;		Returns a list of scan status values, as an integer vector of length
;		NSCAN, for all the scans specified.  See SPSCAN_DEFINE for listing of
;		possible values.
;	PD_STATUS
;		Returns a list of scan area detector status values, as an integer vector
;		of length NSCAN, for all the scans specified.  See SPSCAN_DEFINE for 
;		listing of possible values.
;	DETAILS
;		Returns a list of scan headers, as a string vector of length NSCAN, for
;		all the scans specified.
;	MOTORS
;		Returns a list of active motors for all the scans specified.  This list
;		is a string array of dimensions [NSCAN,NMAX] where NMAX is the maximal
;		number of motors present.  For scans where the number of motors is less
;		than NMAX, the excessive entries are filled with null strings.
;	DATA_POINTS
;		Returns a list of the number of data points for all the scans specified,
;		as a long integer vector of length NSCAN.
;	ALPHA
;		Returns a list of the (starting) ALPHA values for all the scans
;		specified, as a float vector of length NSCAN.
;	BETA
;		Same as ALPHA, for the BETA values.
;	DTH
;		Same as ALPHA, for the DTH values.
;	COUNT
;		Returns the number of scans in the list
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward.  Calls SCAN_FIELD_READ.  Also calls APPROX, ARREQ, 
;		DEFAULT, HOW_MANY, ONE_OF, STREQ, STRPARSE_MM and TABULATE, from MIDL.
; MODIFICATION HISTORY:
;		Created 25-JUN-2006 by Mati Meron.
;		Modified 15-AUG-2006 by Mati Meron.  Internal changes.
;		Modifed 5-MAY-2010 by Mati Meron.  Added keyword VPD and additional 
;		return variables, namely: PD_STATUS, DATA_POINTS, ALPHA, BETA and DTH.
;		Modified 20-MAY-2010 by Mati Meron.  Added keyword ALT_SHOW.
;		Modified 15-NOV-2010 by Mati Meron.  Format changes.
;-

	on_error, 1

	lo = [3,5,7]
	hi = [4,6,8]
	hkl = ['H','K','L']
	huge = (machar()).xmax

	res = Scan_field_read(Default(snum,0,/dtyp),'stype',/uni,list=lis,_extra=_e)
	sta = Scan_field_read(lis,'stat')
	pds = Scan_field_read(lis,'pdstat')
	det = Scan_field_read(lis,'shead')
	shtyp = One_of(sho,sha,/nozero)
	shofl = shtyp ge 0
	motfl = arg_present(mot) or shofl
	datfl = arg_present(dtp) or shofl
	angfl = [arg_present(alp),arg_present(bet),arg_present(dth)] or shofl

	if motfl then begin
		nsc = n_elements(lis)
		mot = strarr(nsc,4)
		nmot = intarr(nsc)
		for i = 0l, nsc-1 do begin
			len = Strparse_mm(det[i],' 	',tlis)
			if Streq(res[i],'hklscan',7) then begin
				check = Approx(float(tlis[lo]),float(tlis[hi]))
				dum = where(check eq 0,ndum)
				if ndum gt 0 then begin
					mot[i,0] = hkl[dum[0]]
					nmot(i) = 1
				endif
			endif else begin
				if len gt 2 then begin
					for j = 3, len do begin
						if (byte(tlis[j]))[0] gt 64 then begin
							mot[i,nmot[i]] = tlis[j]
							nmot[i] = nmot[i] + 1
						endif
					endfor
				endif
			endelse
		endfor
		nmax = max(nmot)
		mot = mot[*,0:(nmax-1)>0]
		if nmax le 1 then mot = reform(mot,nsc,1)
	endif

	if datfl then dtp = (Scan_field_read(lis,'ncr'))[*,1]

	if not Arreq(angfl,[0,0,0]) then begin
		angs = Scan_field_read(lis,'angs')
		alp = angs[*,0]
		bet = angs[*,1]
		dth = angs[*,2]
	endif

	cnt = n_elements(res)
	vast = How_many(fir=val,sec=str,whi=whi)
	if vast gt 0 then begin
		if keyword_set(vpd) then begin
			if whi/2 then dum = where(((pds-2) > 0) mod 2,cnt) $
			else dum = where(pds mod 2,cnt)
		endif else begin
			if whi/2 then dum = where(sta eq 1,cnt) $
			else dum = where(sta mod 2,cnt)
		endelse
		if cnt gt 0 then begin
			res = res[dum]
			lis = lis[dum]
			sta = sta[dum]
			pds = pds[dum]
			det = det[dum]
			if motfl then mot = mot[dum,*]
			if datfl then dtp = dtp[dum]
			if angfl[0] then alp = alp[dum]
			if angfl[1] then bet = bet[dum]
			if angfl[2] then dth = dth[dum]
		endif else begin
			res = ''
			lis = -1l
			sta = 0
			pds = 0
			det = ''
			if motfl then mot = ''
			if datfl then dtp = 0
			if angfl[0] then alp = huge
			if angfl[1] then bet = huge
			if angfl[2] then dth = huge
		endelse
	endif

	if shofl and cnt gt 0 then begin
		print
		if shtyp then begin
			Tabulate, lis, res, sta, dtp, alp, bet, dth, $
			form=['i4','a12','i4','i4','f8.3','f8.3','f8.3'], $
			head=['Scan #','Type','Status','Points','Alpha','Beta','Dth']
		endif else begin
			m0 = mot[*,0]
			if nmax gt 1 then m1 = mot[*,1]
			if nmax gt 2 then m2 = mot[*,2]
			if nmax gt 3 then m3 = mot[*,3]
			Tabulate, lis, res, sta, dtp, m0, m1, m2, m3, $
			form=['i4','a12','i4','i4','a8','a8','a8','a8'], $
			head=['Scan #','Type','Status','Points',$
			'Mot. 1','Mot. 2','Mot. 3','Mot. 4']
		endelse
	endif

	return, res
end