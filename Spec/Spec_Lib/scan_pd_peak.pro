Function Scan_PD_peak, snum, fnum, region = reg, mean = mea, range = rng, $
	flist = wfnum, title = tit, _extra = _e

;+
; NAME:
;		SCAN_PD_PEAK
; VERSION:
;		8.16
; PURPOSE:
;		Returns the location(s) of the maximal (or central) pixel within the
;		selected frame(s) in a PD scan.
; CATEGORY:
;		SPEC PD processing.
; CALLING SEQUENCE:
;		Result = SCAN_PD_PEAK( SNUM [, FNUM] [, keywords])
; INPUTS:
;	SNUM
;		3 possible options:
;		1)	Single scan number.
;		2)	Nothing, in which case the user is prompted to choose a frame to 
;			read.
;		3)	A complete preread image, either in the [M,N] or the [4,M,N] format.
; OPTIONAL INPUT PARAMETERS:
;	FNUM
;		Frame number(s) within the scan.  If not given, defaults to all frames.
;		Relevant only to SNUM option (1).
; KEYWORD PARAMETERS:
; 	REGION
; 		Specifies search region, in the format [Xmin,Ymin,Xmax,Ymax].  If not 
; 		provided, the search is within the whole frame, for each frame.  A value
; 		of -1 is equivalent to 'not given", useful for programing.
; 		For snum option (1) REGION can also be provided as an [4,n] array, 
; 		where n >= the number of frames in the scan.  In such case REGION[*,i]
; 		is applied to frame i.
;	/MEAN
;		Switch.  If set, the centroid location is returned insted of the peak
;		location.
;	RANGE
;		Provides range around the peak location within which the centroid is
;		calculated.  Can be given as a scalar (in which case same range is used
;		for both dimensions) or a 2 element vector, containing separate ranges
;		for the horizontal and vertical dimensions.  The centroid, within each
;		dimension, is calculated over [peak-range:peak+range].
;		RANGE is only meaningful if MEAN is set.  If not provided, RANGE
;		defaults to the full frame.	
;	FLIST
;		Optional output, see below.
;	TITLE
;		Optional output, see below.
;	_EXTRA
;		A formal keyword, used to transfer additional keywords to embedded
;		routines.
; OUTPUTS:
;		Returns the location(s) of maximal (or central) value(s) of the
;		specified frames.  If a single frame is specified the result is a
;		2-element vector else it is a [2,N] array where N is the number of
;		frames, with each [2,i] couple being the coordinates for the
;		corresponding frame.
;
;		Note:	When MEAN is set, the output is of type FLOAT, else it is of
;				type LONG.
; OPTIONAL OUTPUT PARAMETERS:
;	FLIST
;		Returns the list of frames for which the evaluation has been done.
;		The result is a scalar for a single frame, else it is a vector.
;		Note:	In case of SNUM option 2 or 3 FLIST returns -1.
;	TITLE
;		Returns the Pilatus data filename to be used as a plot title, for the
;		use of calling routines.
;		Note:	In case of SNUM option 3 TITLE returns a null string.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None other that the data files must exist.
; PROCEDURE:
;		Straightforward data processing.  Calls SCAN_PD_FPREAD and SCAN_PD_READ.
;		Also calls ARREQ, ARRLOC, CAST and ISNUM, from MIDL.
; MODIFICATION HISTORY:
;		Created 10-JUN-2008 by Mati Meron.
;		Modified 20-JUN-2008 by Mati Meron.  Added keyword TITLE.
;		Modified 25-AUG-2008 by Mati Meron.  Internal changes.
;		Modified 30-OCT-2008 by Mati Meron.  Added keywords MEAN and RANGE.
;		Modified 10-NOV-2008 by Mati Meron.  Streamlined.  Added keyword FLIST.
;		Modified 5-APR-2010 by Mati Meron.  Added keyword REGION and the option
;		of inputing preread data.
;		Modified 10-FEB-2011 by Mati Meron.  Internal changes.
;		Modified 25-DEC-2011 by Mati Meron.  Internal changes re. REGION.
;		Modified 20-AUG-2012 by Mati Meron.  Bug fix.
;-

	on_error, 1

	if keyword_set(mea) then begin
		mfl = 1
		if Isnum(rng) then begin
			rfl = 1
			if n_elements(rng) eq 1 then rng = [rng,rng]
		endif else rfl = 0
	endif else mfl = 0

	regfl = 0
	nreg = n_elements(reg)
	if nreg mod 4 eq 0 then begin
		jreg = nreg/4
		if jreg gt 0 then begin
			wreg = reform(reg,4,jreg)
			regfl = 1
		endif
	endif else if nreg ne 0 and not Arreq(reg,-1) $
	then message, "Region's # of elements must be a multiple of 4!'

	if n_elements(snum) eq 1 then begin
		dat = Scan_PD_fpread( $
		snum,fnum,coo=xy,title=tit,flist=wfnum,nframes=nfnum,_extra=_e)
		span = (size(dat,/dim))[1:2] - 1
		if mfl then begin
			x = reform(xy[0,*,*])
			y = reform(xy[1,*,*])
			if not rfl then begin
				hrn = [0,span[0]]
				vrn = [0,span[1]]
			endif
		endif
		if regfl then begin
			if jreg eq 1 and nfnum gt 1 then begin
				oreg = wreg
				wreg = make_array(4,nfnum,type=Type(oreg))
				for i = 0, 3 do wreg[i,*] = replicate(oreg[i,0],nfnum)
				jreg = nfnum
			endif
			if jreg lt nfnum then message, 'Insufficient region data!'
			wreg[[0,2],*] = 0 > wreg[[0,2],*] < span[0]
			wreg[[1,3],*] = 0 > wreg[[1,3],*] < span[1]
			gmask = fltarr(span+1)
		endif
		res = fltarr(2,nfnum)
		siz = size(reform(dat[0,*,*]))
		for i = 0l, nfnum-1 do begin
			rdat = reform(dat[i,*,*])
			if regfl then begin
				mask = gmask
				mask[wreg[0,i]:wreg[2,i],wreg[1,i]:wreg[3,i]] = 1
				rdat = rdat*mask
			endif
			max = max(rdat,loc,min=min)
			if max gt min then begin
				res[*,i] = Arrloc(loc,siz)
				if mfl then begin
					if rfl then begin
						hrn = 0 > (res[0,i] + rng[0]*[-1,1]) < span[0]
						vrn = 0 > (res[1,i] + rng[1]*[-1,1]) < span[1]
					endif
					pdat = reform(dat[i,hrn[0]:hrn[1],vrn[0]:vrn[1]])
					int = total(pdat)
					res[0,i] = total(x[hrn[0]:hrn[1],vrn[0]:vrn[1]]*pdat)/int
					res[1,i] = total(y[hrn[0]:hrn[1],vrn[0]:vrn[1]]*pdat)/int
				endif
			endif else res[*,i] = (wreg[[0,1],i] + wreg[[2,3],i])/2
		endfor
	endif else begin
		wfnum = -1
		tit = ''
		case (size(snum))[0] of
			0	:	begin
						if n_elements(snum) eq 0 then begin
							if mfl $
							then dat = Scan_PD_read(/raw,tit=tit,_extra=_e) $
							else dat = Scan_PD_read(/jimg,tit=tit,_extra=_e)
						endif else message, 'Multiple inputs not allowed!'
					end
			1	:	message, 'Multiple inputs not allowed!'
			2	:	dat = snum
			3	:	dat = snum
			else:	message, 'Unknown data format!'
		endcase
		siz = size(dat)
		if mfl then begin
			if siz[0] eq 2 then begin
				x = findgen(siz[1])#replicate(1.,siz[2])
				y = replicate(1.,siz[1])#findgen(siz[2])
			endif else begin
				x = reform(dat[0,*,*])
				y = reform(dat[1,*,*])
				dat = reform(dat[2,*,*])
			endelse
		endif else if siz[0] eq 3 then dat = reform(dat[2,*,*])
		span = size(dat,/dim) - 1
		if regfl then begin
			wreg[[0,2]] = 0 > wreg[[0,2]] < span[0]
			wreg[[1,3]] = 0 > wreg[[1,3]] < span[1]
			mask = fltarr(span+1)
			mask[wreg[0]:wreg[2],wreg[1]:wreg[3]] = 1
			rdat = dat*mask
		endif else rdat = dat
		max = max(rdat,loc,min=min)
		if max gt min then begin
			res = Arrloc(loc,size(rdat))
			if mfl then begin
				res = Cast(res,4)
				if rfl then begin
					hrn = 0 > (res[0] + rng[0]*[-1,1]) < span[0]
					vrn = 0 > (res[1] + rng[1]*[-1,1]) < span[1]
				endif else begin
					hrn = [0,span[0]]
					vrn = [0,span[1]]
				endelse
				pdat = dat[hrn[0]:hrn[1],vrn[0]:vrn[1]]
				int = total(pdat)
				res[0] = total(x[hrn[0]:hrn[1],vrn[0]:vrn[1]]*pdat)/int
				res[1] = total(y[hrn[0]:hrn[1],vrn[0]:vrn[1]]*pdat)/int
			endif
		endif else res = (wreg[[0,1]] + wreg[[2,3]])/2
	endelse

	return, reform(Cast(res,3+mfl,3+mfl))
end