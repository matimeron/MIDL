Function Scan_where, snum, stype = stp, motors = mot, $
	pdet = pdt, valid = val, strict = str, $
	arange = arn, brange = brn, drange = drn, prange = prn, abs_val = abv, $
	details= det, data_points= dtp, alpha= alp, beta= bet, dth= dth, count=cnt,$
	show = sho, null = nul, _extra = _e

;+
; NAME:
;		SCAN_WHERE
; VERSION:
;		8.478
; PURPOSE:
;		Searches within a SPEC file for scans of specific type.
; CATEGORY:
;		SPEC file processing
; CALLING SEQUENCE:
;		Result = SCAN_WHERE ( SNUM [, keywords])
; INPUTS:
;	SNUM
;		Scan list.  Either an array of integers or a character scalar or array
;		of any form acceptable by SCAN_LIST_VER.  If not given or if the single
;		value 0 is provided, it translates to "all scans within the file".
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	STYPE
;		Character scalar, the type of scan sought.  If the last character of 
;		STYPE is "*" then all scan types beginning with the prefix are chosen 
;		(for example "asc*" represents both "ascan" and "ascanb"), else only
;		exact matches are chosen.  If "*" is the only character present, all
;		scans are chosen.
;
;		Note:	In prior versions this keyword was TYPE.  The name has been 
;				changed for the sake of consistency with other routines.
;	MOTORS
;		Optional character array, list of motors.
; 	/PDET
; 		Switch.  If set, only area detector (PD) scans are searched.
;	/VALID												|
;		Switch.  If set, only valid scans, i.e. scans	|	If both keywords
;		with an odd STATUS value, are recognized.		|	are used in a call,
;	/STRICT												|	STRICT takes
;		Switch.  Same as VALID but stricter, only scans	|	preference.
;		with STATUS = 1 are recognized. 				|
;	ARANGE
;		Range of acceptable ALPHA values.   If given as a single value, this 
;		value serves as the bottom of the range, with the top being unlimited.
;	BRANGE
;		Same as ARANGE, for BETA values.
;	DRANGE
;		Same as ARANGE, for DTH values.
;	PRANGE
;		Range of acceptable number of data points.  Unlike the previous three
;		ranges, if PRANGE is given as a single value, this value serves as both
;		top and bottom of the range, i.e. only scans with this number of data
;		points are selected.
;	/ABS_VAL
;		Switch.  If set, ARANGE, BRANGE and DRANGE apply to absolute values of
;		the angles.
;	DETAILS
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
;	/SHOW
;		Switch.  If set, a list of the scans satisfying the requirements is
;		printed to the screen.
;	/NULL
;		Switch.  If set and COUNT = 0 (i.e. no scans found), the output and
;		all optional outputs (except for COUNT) are set to !NULL
;	_EXTRA
;		A formal keyword used to pass keywords to SCAN_WHAT.  Not to be used
;		directly.
; OUTPUTS:
;		Returns a list of the numbers of all scan, from within the list SNUM, of
;		type STYPE and (optionally) using the motor(s) MOTORS.  If no matching
;		scans are found, returns -1 (or !NULL, if /NULL is set)
; OPTIONAL OUTPUT PARAMETERS:
;	DETAILS
;		Returns a list of scan headers, for all the scans in the output list,
;		as a string vector.
;	DATA_POINTS
;		Returns a list of the number of data points, for all the scans in the
;		output list, as a long integer vector.
;	ALPHA
;		Returns a list of the (starting) ALPHA values for all the scans in the
;		output list, as a float vector.
;	BETA
;		Same as ALPHA, for the BETA values.
;	DTH
;		Same as ALPHA, for the DTH values.
;	COUNT
;		Returns the number of scans in the output list.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward.  Searches for matches within data provided by 
;		SCAN_WHAT.  Calls ISNUM, STREQ, STRMATCH_MM and TYPE, from MIDL.
; MODIFICATION HISTORY:
;		Created 25-JUN-2006 by Mati Meron.
;		Modified 25-MAR-2010 by Mati Meron.  Internal changes.
;		Rewritten 10-MAY-2010 by Mati Meron.  Added keywords PDET, ARANGE, 
;		BRANGE, DRANGE, PRANGE and ABS_VAL.  Also added additional return 
;		variables, namely: DATA_POINTS, ALPHA, BETA and DTH.
;		Modified 5-FEB-2014 by Mati Meron.  Added keyword SHOW.
;		Modified 15-JUN-2015 by Mati Meron.  Added keyword NULL.
;		Modified 30-NOV-2016 by Mati Meron.  Enabled "*" option for STYPE. 
;-

	on_error, 1
	huge = (machar()).xmax
	abfl = keyword_set(abv)

	if Type(stp) eq 7 then begin
		styps = Scan_what(snum,valid=val,stric=str,vpd=pdt,list=lis,motor=wmot,$
		data=dtp,alpha=alp,beta=bet,dth=dth,count=cnt,details=det,_extra=_e)
		if cnt gt 0 then begin
			if Streq(strmid(stp,0,/rev),'*') then begin
				slen = strlen(stp)-1
				wstp = strmid(stp,0,slen)
			endif else begin
				slen = 0
				wstp = stp
			endelse
			if Streq(wstp,'') then ind = lindgen(cnt) $
			else ind = where(Streq(styps,wstp,slen),cnt)

			if cnt gt 0 then begin
				if n_elements(mot) gt 0 then begin
					mdum = where(mot ne '',nm)
					if nm gt 0 then begin
						cmot = mot[mdum]
						wmot = wmot[ind,*]
						sizm = size(wmot)
						if sizm[0] eq 1 then wmot = reform(wmot,sizm[1],1)
						sizm = size(wmot)
						tlen = sizm[1]*sizm[2]
						tem = reform(wmot,tlen)
						item = lonarr(tlen)
						for i = 0, nm-1 do begin
							dum = Strmatch_mm(cmot[i],tem,/all,num=ndum)
							if ndum gt 0 then item[dum] = 1
						endfor
						check = total(reform(item,sizm[1],sizm[2]),2)
						dum = where(check eq nm,cnt)
						if cnt gt 0 then ind = ind[dum]
					endif
				endif

				if (cnt gt 0) and Isnum(arn) then begin
					if n_elements(arn) eq 1 then ran = [arn,huge] else ran = arn
					walp = alp[ind]
					if abfl then walp = abs(walp)
					dum = where(walp ge min(ran) and walp le max(ran),cnt)
					if cnt gt 0 then ind = ind[dum]
				endif

				if (cnt gt 0) and Isnum(brn) then begin
					if n_elements(brn) eq 1 then ran = [brn,huge] else ran = brn
					wbet = bet[ind]
					if abfl then wbet = abs(wbet)
					dum = where(wbet ge min(ran) and wbet le max(ran),cnt)
					if cnt gt 0 then ind = ind[dum]
				endif

				if (cnt gt 0) and Isnum(drn) then begin
					if n_elements(drn) eq 1 then ran = [drn,huge] else ran = drn
					wdth = dth[ind]
					if abfl then wdth = abs(wdth)
					dum = where(wdth ge min(ran) and wdth le max(ran),cnt)
					if cnt gt 0 then ind = ind[dum]
				endif

				if (cnt gt 0) and Isnum(prn) then begin
					if n_elements(prn) eq 1 then ran = [prn,prn] else ran = prn
					wdtp = dtp[ind]
					dum = where(wdtp ge min(ran) and wdtp le max(ran),cnt)
					if cnt gt 0 then ind = ind[dum]
				endif
			endif
		endif
	endif else message, 'Scan type must be provided!'

	if cnt gt 0 then begin
		res = lis[ind]
		det = det[ind]
		dtp = dtp[ind]
		alp = alp[ind]
		bet = bet[ind]
		dth = dth[ind]
	endif else begin
		if not keyword_set(nul) then begin
			res = -1l
			det = ''
			dtp = 0
			alp = huge
			bet = huge
			dth = huge
		endif else res = (det = (dtp = (alp = (bet = (dth = !null)))))
	endelse

	if keyword_set(sho) and cnt gt 0 then dum = Scan_what(res,/sho)

	return, res
end