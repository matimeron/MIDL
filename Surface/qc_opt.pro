Function Qc_opt, dat, qtry, cut = cut, error = err, status = sta, _extra = _e

;+
; NAME:
;		QC_OPT
; 	VERSION:
;		8.42
; PURPOSE:
;		Finds best fit for Q_crit, for reflectivity data
; CATEGORY:
;		Mathematical, x-ray specific.
; CALLING SEQUENCE:
;		Result = QC_OPT( DAT, QRAN [, keywords])
; INPUTS:
;	DAT
;		Reflectivity data in the standard [3,N] form ([2,N] is acceptable).
;	QTRY
;		Either a scalar value, the initial guess for Q_crit, or a 2-element
;		vector, the initial Q-range within which Q_crit is sought.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
; 	CUT
; 		Scalar, the Q-cut value for the data for fitting purposes.  Default is
; 		4 times the higher of the values in QRAN.
; 	ERROR
; 		Optional output, see below.
; 	_EXTRA
; 		A formal keyword, used to pass additional keywords to QND_EXT.  Not to
; 		be used directly.
; OUTPUTS:
;		Returns a "best fit" Q_crit value for the data DAT.
; OPTIONAL OUTPUT PARAMETERS:
; 	ERROR
; 		Returns the estimated fit error value for the result.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		The DAT input must be of proper form, as delineated above.
; PROCEDURE:
; 		Straightforward, locates the neighborhood of the minimum through calls
; 		to QC_OPT_FUN, then calls DEFAULT, ISNUM and QND_EXT from MIDL.  In some
; 		cases calls itself recursively.
; MODIFICATION HISTORY:
;		Created 25-MAY-2015 by Mati Meron.
;-

	on_error, 1

	n = n_elements(qtry)
	if n eq 0 or n gt 2 then message, 'Bad or missing qtry!'
	qdat = dat[0,*]	
	if n_elements(qtry) eq 1 then begin
		n = n_elements(qdat)
		if n lt 3 then message, 'at least 3 data points needed!'
		i = 1 > value_locate(qdat,qtry) < (n-2)
		if (qtry - qdat[i]) gt (qdat[i+1] - qtry) then i = (i+1) < (n-2)
		i = i + [-1,0,1]
		vals = Qc_opt_fun(qdat[i],dat,_extra=_e)
		done = 0
		repeat begin
			dum = min(vals,j)
			case j of
				0	:	begin
							if i[0] gt 0 then begin
								i = i - 1
								vals = shift(vals,1)
								vals[0] = Qc_opt_fun(qdat[i[0]],dat,_extra=_e)
							endif else begin
								k = [0,1]
								done = 1
							endelse
						end
				1	:	begin
							if vals[0] lt vals[2] then k = [0,1] else k = [1,2]
							done = 1
						end
				2	:	begin
							if i[2] lt n-1 then begin
								i = i + 1
								vals = shift(vals,-1)
								vals[2] = Qc_opt_fun(qdat[i[2]],dat,_extra=_e)
							endif else begin
								k = [1,2]
								done = 1
							endelse
						end
			endcase
		endrep until done
		qran = qdat[i[k]]
		qini = (qran[1] + qran[0])/2.
		step = (qran[1] - qran[0])/4.
		wcut = Default(cut,4.*qran[1],/dtyp)
		dum = where(qdat le wcut)
		cdat = dat[*,dum]
		res = QND_ext('qc_opt_fun',x=qini,par=cdat,step=step,$
			error=err,status=sta,_extra=_e)
	endif else begin
		del = 1e-4
		qran = qtry
		if qran[0] gt qran[1] then qran = reverse(qran)
		qini = (qran[1] + qran[0])/2.
		step = ((qran[1] - qran[0]) > del)/4.
		wcut = Default(cut,4.*qran[1],/dtyp)
		dum = where((qdat le qran[0] or qdat ge qran[1]) and qdat le wcut, ndum)
		if ndum gt 0 then cdat = dat[*,dum] else message, 'Bad range!'
		res = QND_ext('qc_opt_fun',x=qini,par=cdat,step=step,$
			error=err,status=sta,_extra=_e)
		if Isnum(res) then $
			res = Qc_opt(dat,res,cut=cut,error=err,status=sta,_extra = _e)
	endelse

	return, res
end