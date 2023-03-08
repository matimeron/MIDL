Function Bimorph_corv, volt, maxd, weights = wei, status = sta

;+
; NAME:
;		BIMORPH_CORV
; VERSION:
;		8.16
; PURPOSE:
;		Corrects Bimorph mirror voltages. 
; CATEGORY:
;		Bimorph mirror specific.
; CALLING SEQUENCE:
;		Result = BIMORPH_CORV( SNUM [, NSCANS = NSC])
; INPUTS:
;	VOLT
;		Numeric vector, list of voltages to be applied to the consecutive 
;		segments of the mirror.  The number of elements in VOLT must be the same
;		as the number of segments (present in the common block BIMORPH_DAT, 
;		currently set at 16).
; OPTIONAL INPUT PARAMETERS:
;	MAXD
;		Maximal allowed difference between consecutive voltages.  Default is 500
; KEYWORD PARAMETERS:
; 	WEIGHTS
; 		Optional vector of weights for fitting purposes with QCHAIN_GPAR (see
; 		there).  Default is no weights.
; 	STATUS
; 		Optional output, see below.
; OUTPUTS:
; 		Returns a list of voltages in the same format as the input VOLT.  The
; 		returned voltages are corrected (if needed) to asssure that the 
; 		difference between consecutive voltages doesn't exceed MAXD.
; OPTIONAL OUTPUT PARAMETERS:
; 	STATUS
; 		Returns:
; 			0	-	if the voltage correction failed.
; 			1	-	if correction wasn't needed.
; 			3	-	if correction was needed and was performed.
; COMMON BLOCKS:
;		BIMORPH_DAT.  See BIMORPH_INIT for details.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
; 		Corrects voltages as needed to keep differences below maximum allowed
; 		while keeping global curvature (always) and local integrated curvature
; 		(always except for the end elements) constant.  Calls BIMORPH_INIT 
; 		and QCHAIN_GPAR.  Calls DEFAULT, DIF and SIGN, from MIDL.
; MODIFICATION HISTORY:
;		Created 15-JAN-2012 by Mati Meron.
;		Modified 30-JUL-2012 by Mati Meron.  Significant internal changes.
;-

	common bimorph_dat, bexs, nseg, modln, bords, cwei, bmconst
	on_error, 1
	Bimorph_init

	frac = 0.99
	defmaxd = 500.
	wmax = frac*(Default(maxd,defmaxd) < defmaxd)

	cvolt = Cast(volt,4)
	xc = (bords[0:-2] + bords[1:-1])/2.
	dum = Qchain_gpar(fltarr(nseg),xc,gval=bords,cgr=gc,/cur,wei=Default(wei,1))
	dum = Qchain_gpar(fltarr(nseg),xc,gval=bords,cgr=gs,/slo,wei=Default(wei,1))

	sta = (count = 0)
	repeat begin
		ldif = Dif(cvolt,/bac)
		rdif = -Dif(cvolt,/for)
		adif = abs(ldif) > abs(rdif)
		madif = max(adif,i)
		if madif gt wmax then begin
			if i ge nseg/2 and i lt nseg-1 then $
			if adif[i+1] eq adif[i] then i = i+1
			if abs(ldif[i]) ge abs(rdif[i]) then mdif=ldif[i] else mdif=rdif[i]
			delv = Sign(mdif)*(wmax - abs(mdif))
			case i of
				0	:	begin
							ind = i + [0,1]
							corr = [1,-gc[i]/gc[i+1]]*delv
						end
				nseg-1:	begin
							ind = i + [-1,0]
							corr = [-gc[i]/gc[i-1],1]*delv
						end
				else:	begin
							denom = gc[i-1]*gs[i+1] - gs[i-1]*gc[i+1]
							lef = (gc[i]*gs[i+1] - gs[i]*gc[i+1])/denom
							rig = (gc[i-1]*gs[i] - gs[i-1]*gc[i])/denom
							delv = delv/(1 + (lef < rig))
							ind = i + [-1,0,1]
							corr = [-lef,1,-rig]*delv
						end
			endcase
			cvolt[ind] = cvolt[ind] + corr
			count = count+1
		endif else sta = 3
	endrep until sta or count eq 10*nseg
	if count eq 0 then sta = 1

	return, cvolt
end