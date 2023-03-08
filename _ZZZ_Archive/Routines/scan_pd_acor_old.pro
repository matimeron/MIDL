Function Scan_PD_acor_old, snum, fnum, hroi = hroi, oldstyle = old

;+
; NAME:
;		SCAN_PD_ACOR
; VERSION:
;		8.472
; PURPOSE:
;		Returns the area correction factor for GID scans.
; CATEGORY:
;		Surface data processing.
; CALLING SEQUENCE:
;		Result = SCAN_PD_ACOR( SNUM [, FNUM])
; INPUTS:
;	SNUM
;		Single scan number.
; OPTIONAL INPUT PARAMETERS:
;	FNUM
;		Frame number(s) within the scan.  If not given, defaults to all frames.
; KEYWORD PARAMETERS:
;	HROI
;		Two element vector defining horizontal region of interest, in *pixels*.
;		If provided, the width of this region (in mm) serves as alternative
;		S5 slit, and the final slit used in the evaluation will be the smaller
;		of this and the physical slit.
; 	/OLDSTYLE
; 		Switch.  Causes the result to be renormalized, dividing by the sine of
; 		the smallest nonzero DTH angle.  Used for comparisons with old data
; 		normalization only.
; OUTPUTS:
;		Returns the value(s) by which the consecutive data columns in GID scan
;		need to be multiplied to account for changing data collection area.
; OPTIONAL OUTPUT PARAMETERS:
;		None
; COMMON BLOCKS:
;		SPEC_FILE.  See the routine SPEC_FILE_INFO for description.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		The scan must be a valid GID scan, with a 'DET_TH' data column.
; PROCEDURE:
;		The result, for each DTH angle, is the reference field of view (by
;		default at 90 degrees) divided by the field of view at DTH.  For not too
;		small angles this yields sin(DTH).  The parameters required for the
;		field of view evaluation are read directly from the spec file, though
;		S5 may be modified by the HROI input (see above).
;		Calls SCAN_COLUMN, SCAN_PD_FRAMES and SPEC_FILE_CHECK.
;		Calls SHAPE_AREA, SHAPE_CLOSE and SHAPE_EDGE, from MIDL.
; MODIFICATION HISTORY:
;		Created 5-AUG-2016 by Mati Meron.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1

	Spec_file_check, snum, /sing, /pildet, list = wsnum
	cur = fildat.scan[wsnum]

	wfnum = Scan_PD_frames(wsnum,fnum)
	rphi = !dtor*(Scan_column(wsnum,'det_th',sta=sta))[wfnum]
	if sta then begin
		detoff = 32
		l5 = cur.pdist
		l4 = l5 - cur.pdsdist
		s1v= mean(cur.sl_val[0:1,1])
		s1 = mean(cur.sl_val[2:3,1])
		s4 = mean(cur.sl_val[2:3,4])
		s5 = mean(cur.sl_val[2:3,5])
		if keyword_set(hroi) then begin
			as5 = (max(hroi,min=min) - min + 1)*fildat.pdpix/2
			s5 = s5 < as5
		endif else l5 = l5 - detoff
		ralp = !dtor*(cur.angs)[0]
	
		lh = s1v/sin(ralp)
		sden = sqrt((l5-l4)^2 + (s5+s4)^2)
		dv = (l4*s5 + l5*s4)/sden
		psi = asin((s4+s5)/sden)
		omg = asin(s1/sqrt(s1^2 + lh^2))
		ksi = asin(dv/sqrt(s1^2 + lh^2))
		cang = ksi + omg + psi

		res = 0*rphi
		lo = where(rphi lt cang, nlo, comp= hi, ncomp= nhi)
		if nlo gt 0 then begin
			shap = Shape_close([[lh,s1],[-lh,s1],[-lh,-s1],[lh,-s1]])
			carea = 4*dv*s1/cos(psi)
			area = 0*rphi[lo]
			for i = 0, nlo-1 do begin
				iphi = rphi[lo[i]]
				r = [[cos(iphi),-sin(iphi)],[sin(iphi),cos(iphi)]]
				cshap = Shape_edge( $
				shap,[[reform(r##[-l5,s5])],[reform(r##[-l4,-s4])]],/seg)
				cshap = Shape_edge( $
				cshap,[[reform(r##[-l4,s4])],[reform(r##[-l5,-s5])]],/seg)
				area[i] = Shape_area(cshap)
			endfor
			res[lo] = carea/area
		endif
		if nhi gt 0 then res[hi] = $
		(sin(rphi[hi])^2 - sin(psi)^2)/(sin(rphi[hi])*cos(psi)^2)
		if keyword_set(old) then res = res/sin(min(rphi[where(rphi gt 0)]))
	endif else message, 'Not a GID scan!'

	return, res
end