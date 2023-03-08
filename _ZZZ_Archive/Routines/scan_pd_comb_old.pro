Function Scan_PD_comb_old, snum, fnum, verify = ver, orientation = ori, $
	hroi = hroi, vroi = vroi, yoff= yof, norm= nrm, sin_norm= snr, other= oth, $
	regular= reg, show= sho, beta=bet, dth=dth, frames=frm, time=tim, hrof=hrf,$
	title=tit, _extra= _e

;+
; NAME:
;		SCAN_PD_COMB
; VERSION:
;		8.31
; PURPOSE:
;		Reads and combines all the Pilatus Detector files associated with a
;		single SPEC scan.
; CATEGORY:
;		SPEC Input/Output.
; CALLING SEQUENCE:
;		Result = SCAN_PD_COMB( SNUM [, keywords])
; INPUTS:
;	SNUM
;		Single scan number.
; OPTIONAL INPUT PARAMETERS:
;	FNUM
;		Optional list of frame numbers, in any form acceptable by RANGE_PROC.
;		By default, all the frames within SNUM are used.
;
;		Note:	If some of the frame numbers are out of the range present,
;		they're ignored.  If there are no frame numbers within the range
;		present, an error message is issued.
; KEYWORD PARAMETERS:
;	/VERIFY
;		Switch.  If set, the existence of all frames to be processed is
;		verified and bad or missing frames are rejected.
;	ORIENTATION
;		Character input specifying the detector orientation.  Two possible
;		values, "HOR" (for horizontal) and "VER" (for vertical).  First letter
;		is sufficient.  Default is vertical.
;		Alternatively ORIENTATION can be provided as integer input, 0 for
;		horizontal, 1 for vertical.
;	HROI
;		Two element vector defining horizontal region of interest, in *pixels*.
;	VROI
;		Two element vector defining vertical region of interest, in *pixels*.
;	YOFF
;		Numeric scalar, specifying Y_offset along the beam footprint (for GID
;		applications).  If given, results in appropriate offsets in HROI and in
;		DTH.
;	/NORM
;		Switch.  If set, the data is normalized to monitor counts.
;	/SIN_NORM
;		Specifies additional geometric normalization, consisting of multiplying
;		each column by Sin(dth)/Sin(min(dth)).  This corrects for the changing
;		field of view in GID scans.
;		Note:	Currently SIN_NORM is only working for vertical detector
;		orientation.
;	/OTHER
;		Switch.  Specifies the data is *not* a scan on angle.  Used with time
;		series.
;	/REGULAR
;		Switch.  If set, the result is formed into the "regular" [4,*,*] format.
;		The x-y coordinates depend on detector orientation as follows:
;			Vertical orientation	-	DTH : Vertical pixel number.
;			Horizontal orientation	-	Horizontal pixel number : BETA
;	/SHOW
;		Switch.  If set, and more than one valid frame is present, image of the
;		data is displayed to the screen.
;	BETA
;		Optional output, see below
;	DTH
;		Optional output, see below.
;	HROF
;		Optional output, see below.
;	FRAMES
;		Optional output, see below.
;	TIME
;		Optional output, see below.
;	TITLE
;		Optional output, see below.
;	_EXTRA
;		A formal keyword, used to transfer additional keywords to embedded
;		routines.  This includes (among others) the following keywords,
;		affecting data readout:
;
;			/BAD
;				Switch.  If set, faulty (very high count rate) pixels are
;				removed from the data.
;
;		See SCAN_PD_READ for more details.
; OUTPUTS:
;		Returns an array (type FLOAT) of dimension [NP,NL,2] where NP is the
;		number of PD files associated with the scan and NL is the long dimension
;		of the PD (for "HOR" orientation the dimensions are [NL,NP,2]).  The
;		[*,*,0] page contains the data and the [*,*,1] page contains the
;		statistical errors.  Each column (row, in case of "HOR" orientation)
;		is the result of a single PD file integrated across the short PD
;		dimension.
;
;		Note:	NL may be smaller then the full PD length if HROI, VROI are used
;		
;		Note:	The output format is different if the keyword REGULAR is used,
;				see above.
; OPTIONAL OUTPUT PARAMETERS:
;	BETA
;		Returns the Beta angle for the data.
;	DTH
;		Same as BETA, for the Dth angle.
;
;		Note:	For vertical orientation BETA is a scalar and DTH is a vector
;				of length NP.  For horizontal orientation the situation is
;				reversed.
;	HROF
;		If YOFF is provided, RFOF returns the extreme values of HROI offset, 
;		else it is undefined.
;	FRAMES
;		Returns a list of the numbers of processed frames.
;	TIME
;		Returns the times corresponding to the processed frames.  The times are
;		counted from the beginning of the scan with the time attributed to each
;		frame being the completion time.
;	TITLE
;		Returns the Pilatus data filename to be used as a plot title, for the
;		use of calling routines.
; COMMON BLOCKS:
;		SPEC_FILE.  See the routine SPEC_FILE_INFO for description.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None other that the data files must exist.
; PROCEDURE:
;		Straightforward, reads the data using parameters stored in the FILDAT
;		structure in the common block.  Calls IMG_INT, SCAN_COLUMN,
;		SCAN_PD_CENTER, SCAN_PD_FPREAD, SCAN_PD_FTOD and SCAN_PD_FRAMES.  Calls 
;		ERREST from SURF_LIB.  Also calls DISPLAY_MM, ISNUM and TYPE, from MIDL.
; MODIFICATION HISTORY:
;		Created 30-OCT-2007 by Mati Meron.
;		Modified 10-NOV-2007 by Mati Meron.  Added option of partial frame range
;		Modified 25-APR-2008 by Mati Meron.  Internal changes.
;		Modified 20-JUN-2008 by Mati Meron.  Internal changes.
;		Modified 15-AUG-2008 by Mati Meron.	 Internal changes.
;		Modified 25-AUG-2008 by Mati Meron.	 Internal changes.
;		Modified 10-NOV-2008 by Mati Meron.  Internal changes.
;		Modified 5-MAR-2009 by Mati Meron.  Added keyword SIN_NORM.
;		Modified 25-JUN-2009 by Mati Meron.  Added keyword OTHER.
;		Modified 25-OCT-2009 by Mati Meron.  Added keyword TITLE.
;		Modified 5-NOV-2009 by Mati Meron.  Internal changes.
;		Modified 5-FEB-2010 by Mati Meron.  Added keyword VERIFY.
;		Modified 25-MAR-2010 by Mati Meron.  Common block update.
;		Modified 25-MAR-2011 by Mati Meron.  Updated error calculations for APEX
;		Modified 5-APR-2011 by Mati Meron.  Added keyword YOFF.
;		Modified 15-AUG-2012 by Mati Meron.  Internal changes to enable proper
;		functioning at DTH = 0.
;		Modified 10-APR-2013 by Mati Meron.  Added keyword REGULAR for 
;		alternative output format.
;		Modified 5-NOV-2013 by Mati Meron.  Added keyword HROF.
;		Modified 20-NOV-2013 by Mati Meron.  Internal changes.
;		Modified 1-JAN-2014 by Mati Meron.  Added keywords FRAMES and TIME.
;		Modified 15-MAR-2014 by Mati Meron.  Bug fix.
;		Modified 30-MAR-2014 by Mati Meron.  Internal changes.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1

	cent = Scan_PD_center(snum,ori=ori,dim=dim,/sing)
	cur = fildat.scan[snum]
	apfl = (cur.pdstat - 3)/2
	wfnum = (frm = Scan_PD_frames(snum,fnum,ver=ver,nframes=nfnum,_extra=_e))
	sec = Scan_column(snum,'seconds',/const,stat=sta)
	if sta then tim = sec*(frm+1) $
	else message, 'Non-constant time interval', /con
	ofl = keyword_set(oth)
	lo = 0l

	case ori of
		0	:	begin
					if (cur.varan)[1] or ofl then begin
						bet = (Scan_column(snum,0))[wfnum]
						dth = (cur.angs)[2]
						if keyword_set(hroi) then begin
							lo = lo > hroi[0]
							hi = (dim[0] - 1) < hroi[1]
							hdim = hi - lo + 1
						endif else hdim = dim[0]
						res = fltarr(hdim,nfnum,2)
					endif else message, 'Data problems!'
				end
		1	:	begin
					if (cur.varan)[2] or ofl then begin
						bet = (cur.angs)[1]
						dth = (Scan_column(snum,0))[wfnum]
						if keyword_set(vroi) then begin
							lo = lo > vroi[0]
							hi = (dim[1] - 1) < vroi[1]
							vdim = hi - lo + 1
						endif else vdim = dim[1]
						res = fltarr(nfnum,vdim,2)
						if Isnum(yof) then begin
							wof = yof[0]
							if wof ne 0 then begin
								xfof = Scan_PD_ftod($
								snum,wfnum,/ver,yfot=wof,/round,dang=dphi)
								dth = dth + dphi
								fof = transpose([[xfof],[replicate(0,nfnum)]])
								hrf = reform(fof[0,[0,nfnum-1]])
							endif
						endif
					endif else message, 'Data problems!'
				end
	endcase

	if keyword_set(nrm) then begin
		ncol = ''
		if Type(nrm) eq 7 then ncol = nrm $
		else if nrm eq 1 then ncol = 'monc'
		if ncol ne '' then begin
			col = Scan_column(snum,ncol)
			nco = Total(col)/(cur.ncr[1]*col[wfnum])
		endif else nco = replicate(1.,nfnum)
	endif else nco = replicate(1.,nfnum)
	if keyword_set(snr) and ori then begin
		snco = abs(sin(!dtor*dth))
		mnsco = min(snco[where(snco ne 0)])
		nco = nco*(snco > mnsco)/mnsco
	endif
	
	dat = Scan_PD_fpread($
	snum,wfnum,ver=ver,ori=ori,hroi=hroi,vroi=vroi,foff=fof,title=tit,_extra=_e)
	siz = size(dat)
	if siz[0] eq 2 then begin
		dat = reform(dat,siz[1],siz[2],1)
		siz = size(dat)
	endif
	sfl =  siz[2:3] eq 1
	for i = 0l, nfnum-1 do begin
		pdat = reform(dat[i,*,*])
		if sfl[0] then pdat = reform(pdat,1,siz[3])
		if sfl[1] then pdat = reform(pdat,siz[2],1)
		if ori then begin
			idat = nco[i]*Img_int(pdat,/xy_int,/image,emode=apfl,_extra=_e)
			res[i,*,0] = idat[1,*]
			res[i,*,1] = idat[2,*]
		endif else begin
			idat = nco[i]*Img_int(pdat,/z_int,/image,emode=apfl,_extra=_e)
			res[*,i,0] = idat[1,*]
			res[*,i,1] = idat[2,*]
		endelse
	endfor

	if keyword_set(reg) then begin
		regfl = 1
		pres = res
		if ori then begin
			res = fltarr(4,nfnum,vdim)
			res[0,*,*] = dth#replicate(1.,vdim)
			res[1,*,*] = replicate(1.,nfnum)#(lo + findgen(vdim))
			res[2,*,*] = pres[*,*,0]
			res[3,*,*] = pres[*,*,1]
		endif else begin
			res = fltarr(4,hdim,nfnum)
			res[0,*,*] = (lo + findgen(hdim))#replicate(1.,nfnum)
			res[1,*,*] = replicate(1.,hdim)#bet
			res[2,*,*] = pres[*,*,0]
			res[3,*,*] = pres[*,*,1]
		endelse
	endif else regfl = 0

	if keyword_set(sho) and nfnum gt 1 and (not sfl[1]) then begin
		if regfl then Display_mm, res, /auz, tit = tit, _extra = _e $
		else Display_mm, res[*,*,0], /auz, tit = tit, _extra = _e
	endif

	return, res
end