Pro Spec_file_info_old, filename = fnam, new = new, old_pars = olp, details = dtl, $
	ad_check = ach, _extra = _e

;+
; NAME:
;		SPEC_FILE_INFO
; VERSION:
;		8.14
; PURPOSE:
;		Reads a SPEC data file and fills a structure with information pertaining
;		to the file.
; CATEGORY:
;		SPEC Input/Output.
; CALLING SEQUENCE:
;		SPEC_FILE_INFO [, keywords ]
; INPUTS:
;		None.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	FILENAME
;		Char. value, the name of the data file.  If not given, the last SPEC
;		file accessed is used, unless /NEW is set.
;	/NEW
;		Switch.  Forces choice of a new file.  Equivalent to setting EXS (see
;		common block) to zero.
;	/OLD_PARS
;		Switch.  If set, an old list of parameters (pre 2004) is used.
;	/DETAILS
;		Switch.  If set, the file name and warning information (if any was
;		generated) are printed.
;	/AD_CHECK
;		Switch.  If set, the dimensions and pixel size of the AD are checked for
;		 each scan where it is present, else default dimensions are assumed.   
;	_EXTRA
;		A formal keyword used to pass keywords to imbedded routines.  Not to
;		be used directly.
; OUTPUTS:
;		None, other that that the common block SPEC_FILE is modified.
; OPTIONAL OUTPUT PARAMETERS:
;		None
; COMMON BLOCKS:
;		SPEC_FILE.  Contains:
;			EXS		:	Status variable, possible values are:
;							0	:	No file defined.
;							1	:	OK.
;			FILDAT	:	Structure of type SPFILE, see SPFILE__DEFINE for details
;			N_S		:	Integer constant, used in the structure definitions.
;			N_M		:	Integer constant, used in the structure definitions.
;			N_L		:	Integer constant, used in the structure definitions.
;			N_SMAX	:	Integer constant, used in the structure definitions.
;			ITOP	:	Integer constant, maximal size for AD images.
; SIDE EFFECTS:
;		None other than modification of the common block.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward.  Calls READ_BIS for APEX files.  Uses DEFAULT, 
;		FILE_GET, FNAMPARSE, RASCLINE, STREQ, STRMATCH_MM, STRPARSE_MM and TYPE,
;		from MIDL, and SDEP from IMPORTS.
; MODIFICATION HISTORY:
;		Created 5-SEP-2002 by Mati Meron.
;		Modified 25-MAR-2003 by Mati Meron.
;		Rewritten 5-APR-2003 by Mati Meron.
;		Updated 15-MAY-2004 by Mati Meron.
;		Updated 15-OCT-2005 by Mati Meron.
;		Updated 10-DEC-2005 by Mati Meron.  New LD standard implemented.
;		Updated 20-JUN-2006 by Mati Meron.  Small internal changes.
;		Updated 15-JUL-2006 by Mati Meron.  Increased the values of N_M, N_L.
;		Updated 10-AUG-2007 by Mati Meron.  PD support introduced and keyword
;		CHECK_DATA added.
;		Updated 5-NOV-2007 by Mati Meron.  Removed the keyword CHECK_DATA (no
;		longer needed) and added support for partial slits.
;		Updated 10-FEB-2008 by Mati Meron.  Internal changes.
;		Modified 15-AUG-2008 by Mati Meron.  Internal changes.
;		Modified 15-DEC-2008 by Mati Meron.  Internal changes.
;		Modifed 1-JUN-2009 by Mati Meron.  Internal changes.
;		Modifed 5-JUN-2009 by Mati Meron.  Internal changes.
;		Modified 5-AUG-2009 by Mati Meron.   Added APEX support.
;		Modified 5-FEB-2010 by Mati Meron.  Renamed keyword VERBOSE to DETAILS,
;		in order to prevent keyword crosstalk.
;		Modified 20-MAR-2010 by Mati Meron.  Internal changes.
;		Modified 25-MAR-2010 by Mati Meron.  Common block update.
;		Modified 5-APR-2010 by Mati Meron.  Internal changes.
;		Modified 20-APR-2010 by Mati Meron.  Internal changes.
;		Modified 5-FEB-2011 by Mati Meron.  Internal changes.
;		Modified 10-JUN-2011 by Mati Meron.  Added keyword AD_CHECK.
;		Modified 15-AUG-2011 by Mati Meron.  Internal changes.
;		Modified 10-OCT-2011 by Mati Meron.  Added a Camera option.
;		Modified 25-OCT-2011 by Mati Meron.  Added Bimorph voltages' readout.
;		Modified 10-DEC-2011 by Mati Meron.  Internal changes.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1

	if keyword_set(new) then exs = 0 else exs = Default(exs,0,/dtyp)
	warn = ["There ain't no such file!","", "No data!","File may be corrupted!"]
	line = ''
	chnam =	'Tot_chan'
	ppan = ['In_Rot','Out_Rot','Det_Th','']
	ppgx = ['GOut_Ht', 'GOut_Rot', 'XOut_Ht', 'XOut_Rot']
	pdfar = 2e3
	del = 1e-3
	dsep = sdep(/ds)
	acfl = keyword_set(ach)

	if Type(fnam) eq 7 or not exs then begin
		n_s = 8l
		n_m = 32l
		n_l = 128l
		n_smax = 2048l
		itop = 1024l
		fildat = {spfile}
		wfnam = File_get($
			fnam,/read,stat=exs,title='Please select a SPEC file',_extra=_e)
		if exs then begin
			fildat.status = 2
			fildat.name = wfnam
			openr, spcun, wfnam, /get_lun
			fildat.size = (fstat(spcun)).size
			free_lun, spcun
			if fildat.size gt 0 then begin
				gofl = 3b
				start = 0l
			endif else gofl = 0b
			if keyword_set(olp) then ppnam = $
			['Chi',			'In_Ht',	'In_Rot',	'Sample_H', $
			'Det_Th',		'Out_Ht',	'Out_Rot',	'Two_Thet', $
			'Phi',			'an_Sam_H',	'Analyzer',	'Chan/mm', $
			'Spec_chan',	'Spec_oh',	'Spec_or',	'Samp_det_dist', $
			'Calpha',		'Clambda'] $
			else ppnam = $
			['Chi',			'Phi',			'In_Ht',		'In_Rot',	$
			'Sample_H', 	'Sample_X',		'Two_Thet', 	'Det_Th',	$
			'Absorber',		'Out_Th',		'Out_Ht',		'Out_Rot',	$
			'GOut_Ht',		'GOut_Rot',		'XOut_Ht',		'XOut_Rot', $
			'an_Sam_H',		'Analyzer',		'cpm', 			'ns',		$
			'Theta',		'dtoffset',		'ROI_lo',		'ROI_hi', 	$
			'calib_slp',	'calib_int',	'nd',			'sdd',		$
			'Mir1Y',		'Mir2Y',		'CCD_X',		'CCD_Y']
			fildat.npp = n_elements(ppnam)
			fildat.pp_nam[0:fildat.npp - 1] = ppnam
			for i = 0, 3 do begin
				if ppan[i] ne '' then fildat.pp_an[i]=Strmatch_mm(ppan[i],ppnam)
				fildat.pp_gx[i] = Strmatch_mm(ppgx[i],ppnam)
			endfor
			fildat.nsl = n_s
			fildat.sl_nam = 'S' + string(indgen(n_s),form='(i1)')
		endif else gofl = 0b
	endif else begin
		wfnam = fildat.name
		openr, spcun, wfnam, /get_lun
		fsize = (fstat(spcun)).size
		free_lun, spcun
		if fsize gt fildat.size then begin
			if fildat.nscan gt 0 then begin
				if fildat.scan[fildat.nscan].stat ne 1 then begin
					start = fildat.scan[fildat.nscan].ptr[0]
					fildat.nscan = fildat.nscan - 1
				endif else start = fildat.size
				gofl = 1b
			endif else gofl = 3b
			fildat.size = fsize
		endif else gofl = 0b
	endelse

	if gofl then begin
		ptarr = Rascline(wfnam,start=start,lin=sparr,count=nl,call=2)
		sparr = strtrim(sparr,1)
		slin = [where(Streq(sparr,'#S',2),nslin),nl]
		if nslin gt 0 and not fildat.status then fildat.status = 1
		jend = slin[1:*] - slin - 1

		if gofl eq 3 and slin[0] ne 0 then begin
			if nslin eq 0 then kend = nl - 1 else kend = slin[0] - 1
			fildat.scan[0].ptr[n_s-1] = ptarr[kend]
			fildat.scan[0].lptr[n_s-1] = kend
			stem= sparr[0:kend]
			jd = (where(Streq(stem,'#D',2),njd))[0]
			if njd eq 1 then fildat.datime = strtrim(strmid(stem[jd],3),1)
			jz = (where(Streq(stem,'#Z',2),njz))[0]
			if njz eq 1 then begin
				ndum = Strparse_mm(strmid(stem[jz],3),' 	',nlis) < (n_s - 1)
				vdum = Strparse_mm(stem[jz+1],' 	',vlis) < (n_s - 1)
				if ndum eq vdum then begin
					fildat.nzp = ndum + 1
					fildat.zp_nam[0:ndum] = strtrim(nlis,2)
					fildat.zp_val[0:vdum] = float(vlis)
				endif
			endif
			gofl = 1b
		endif

		for i = 0l, nslin-1 do begin
			ptem = ptarr[slin[i]:slin[i+1]-1]
			stem = sparr[slin[i]:slin[i+1]-1]
			dum = Strparse_mm(stem[0],' 	',lis)
			isc = long(lis[1])
			if (isc - fildat.nscan) ne 1 then fildat.status = 3
			if isc gt fildat.nscan then cur = fildat.scan[isc] else continue
			fildat.nscan = isc
			cur.stat = 2
			cur.shead = strmid(strjoin(lis,' '),1)
			cur.stype = lis[2]
			cur.ptr[0] = ptem[0]
			cur.lptr[0] = 0
			cur.ptr[n_s-1] = ptem[jend[i]]
			cur.lptr[n_s-1] = jend[i]

			jl = (where(Streq(stem,'#L',2),njl))[0]
			if njl eq 1 and jl lt jend[i] then begin
				cur.ptr[1] = ptem[jl]
				cur.lptr[1] = jl

				jd = (where(Streq(stem,'#D',2),njd))[0]
				if njd eq 1 and jd lt jl then $
				cur.sdatime = strtrim(strmid(stem[jd],3),1)

				ja = (where(Streq(stem,'#A',2),nja))[0]
				if nja eq 1 and ja lt jl then begin
					dum = Strparse_mm(stem[ja],' 	',lis)
					if dum ge 2 then cur.nabso= long(lis[2]) else cur.nabso= 0
				endif

				jw = (where(Streq(stem,'#W',2),njw))[0]
				if njw eq 1 and jw lt jl then begin
					dum = Strparse_mm(stem[jw],' 	',lis)
					if dum lt 2 then dum = Strparse_mm(stem[jw+1],' 	',lis)
					cur.LAMBDA = float(lis[dum])
				endif

				jg0 = (where(Streq(stem,'#G0',3),njg0))[0]
				if njg0 eq 1 and jg0 lt jl then begin
					dum = Strparse_mm(stem[jg0],' 	',lis)
					cur.g_l[1:3] = float(lis[6:8])
				endif

				jq = (where(Streq(stem,'#Q',2),njq))[0]
				if njq eq 1 and jq lt jl then begin
					dum = Strparse_mm(stem[jq],' 	',lis)
					qtem = float(lis[1:3])
					dum = where(abs(qtem) le Toler(),ndum)
					if ndum gt 0 then qtem[dum] = 0
					cur.q_val[1:3] = qtem
				endif

				jp = (where(Streq(stem,'#P',2),njp))[0]
				if njp gt 0 and jp lt jl then begin
					ind = jp + 2*lindgen(njp)
					phstr = strjoin(stem[ind],' ')
					plstr = strjoin(stem[ind+1],' ')
					hlen = Strparse_mm(phstr,' 	',phead) + 1
					llen = Strparse_mm(plstr,' 	',plist) + 1
					if hlen eq llen then begin
						cur.pplen = hlen
						if cur.pplen ne fildat.scan[isc-1].pplen then begin
							for k = 0l, fildat.npp - 1 do begin
								l = Strmatch_mm(fildat.pp_nam[k],phead,num=num)
								if num then begin
									cur.pp_set[k] = 1
									cur.pp_ind[k] = l
								endif
							endfor
							fildat.pp_exs = fildat.pp_exs or cur.pp_set
							for k = 0l, fildat.nsl - 1 do begin
								l = Strmatch_mm(fildat.sl_nam[k],phead,2,$
									num=num,/all)
								case num of
									2	:	begin
												scheck = strmid(phead[l[0]],2,1)
												if Streq(scheck,'T') then begin
													cur.sl_set[k] = 3
													cur.sl_ind[0:1,k] = l
												endif
												if Streq(scheck,'L') then begin
													cur.sl_set[k] = 5
													cur.sl_ind[2:3,k] = l
												endif
											end
									4	:	begin
												cur.sl_set[k] = 1
												cur.sl_ind[*,k] = l
											end
									else:
								endcase
							endfor
							fildat.sl_exs = fildat.sl_exs or cur.sl_set
							l = Strmatch_mm(chnam,phead,num=num)
							if num then cur.ch_ind = l
						endif else begin
							cur.pp_set = fildat.scan[isc-1].pp_set
							cur.pp_ind = fildat.scan[isc-1].pp_ind
							cur.sl_set = fildat.scan[isc-1].sl_set
							cur.sl_ind = fildat.scan[isc-1].sl_ind
							cur.ch_ind = fildat.scan[isc-1].ch_ind
							for k = 0l, 3 do begin
								l = Strmatch_mm(ppgx[k],phead,num=num)
								if num then begin
									cur.pp_set[fildat.pp_gx[k]] = 1
									cur.pp_ind[fildat.pp_gx[k]] = l
								endif else begin
									cur.pp_set[fildat.pp_gx[k]] = 0
									cur.pp_ind[fildat.pp_gx[k]] = 0
								endelse
							endfor
							fildat.pp_exs = fildat.pp_exs or cur.pp_set
							if min(fildat.pp_gx) ge 0 then $
							cur.outmode = total([2,4]* $
							(total(cur.pp_set[fildat.pp_gx],1,/int)/2),/int)
							if cur.outmode ne 6 then cur.outmode= cur.outmode+1
						endelse
						ppset = where(cur.pp_set,nset)
						if nset gt 0 then cur.pp_val[ppset] = $
							plist[cur.pp_ind[ppset]]
						slset = where(cur.sl_set eq 1,nset)
						if nset gt 0 then cur.sl_val[*,slset] = $
							plist[cur.sl_ind[*,slset]]
						slset = where(cur.sl_set eq 3,nset)
						if nset gt 0 then begin
							cur.sl_val[0:1,slset] = plist[cur.sl_ind[0:1,slset]]
							cur.sl_val[2:3,slset] = 999
						endif
						slset = where(cur.sl_set eq 5,nset)
						if nset gt 0 then begin
							cur.sl_val[2:3,slset] = plist[cur.sl_ind[2:3,slset]]
							cur.sl_val[0:1,slset] = 999
						endif
						if cur.ch_ind gt 0 then cur.chan = plist[cur.ch_ind]
						for k = 0l, 3 do begin
							l = Strmatch_mm(ppan[k],phead,num=num)
							if num then cur.angs[k] = plist[l]
						endfor
					endif
				endif

				jm = (where(Streq(stem,'#M0',3),njm))[0]
				if njm gt 0 and jm lt jl then begin
					cur.mirfl = 1
					mlin = stem[jm]
					while 1 do begin
						jm = jm+1
						if Streq(stem[jm],'#',1) then break
						mlin = mlin + ' ' + stem[j]
					endwhile
					dum = Strparse_mm(mlin,' 	',lis)				
					cur.mirval = lis[1:*]
				endif

				ss1 = strmid(stem,0,1)
				j = jl
				datfl = 1
				while j lt jend[i] and datfl gt 0 do begin
					j = j + 1
					datfl = strpos(' .+-0123456789',ss1[j])
					if datfl gt 0 then begin
						if not cur.stat then begin
							cur.stat = 1
							cur.ptr[2] = ptem[j]
							cur.lptr[2] = j
							cur.ncr[0] = $
							Strparse_mm(stem[j],' 	') + 1
							cur.ncr[1] = 0l
						endif
						cur.ncr[1] = cur.ncr[1] + 1
					endif
				endwhile

				if j lt jend[i] and datfl eq 0 then begin
					j = j + 1
					dum = Strparse_mm(stem[j],' 	',lis)
					if Streq(lis[0],'Peak') then begin
						cur.peak_loc = float(lis[2])
						cur.peak_I = float(lis[4])
						cur.COM = float(lis[7])
						cur.FWHM = float(lis[10])
						cur.CEN = float(lis[12])
					endif else if cur.stat $
					then cur.stat = 3
				endif else if cur.stat $
				then cur.stat = 3
				if j lt jend[i] and cur.LAMBDA eq 0 then begin
					j = j + 1
					jnx = (where(Streq(stem[j:jend[i]],'#',1)))[0]
					if jnx ne 0 then begin
						if jnx lt 0 then jnx = jend[i] + 1 else jnx = j + jnx
						klm = (where(Streq(stem[j:jnx-1],'lambda',3),njnx))[0]
						if njnx eq 1 then begin
							dum = Strparse_mm(stem[j+klm],' =	',lis)
							cur.LAMBDA = float(lis[1])
						endif
					endif
				endif

				if cur.stat then begin
					dum = Strparse_mm(stem[jl],' 	',head)
					head = head[1:*]
					ncmp = [cur.ncr[0],dum,dum]
					jn = (where(Streq(stem,'#N',2),njn))[0]
					if njn eq 1 and jn lt jl then begin
						dum = Strparse_mm(stem[jn],' 	',lis)
						if dum ge 1 then ncmp[2] = long(lis[1])
					endif
					if min(ncmp,max=max) lt max then cur.stat = 5
					dum = Strmatch_mm(head[0],ppan)
					if dum ge 0 then begin
						sdat = $
						strtrim(stem[cur.lptr[2]:cur.lptr[2]+cur.ncr[1]-1],1)
						dlen = Strpos(sdat,' ')
						col = fltarr(cur.ncr[1])
						for k = 0, cur.ncr[1]-1 do $
						col[k] = Strmid(sdat[k],0,dlen[k])
						cur.angs[dum] = col[0]
						if (max(col,min=min) - min) ge del $
						then cur.varan[dum] = 1
					endif
				endif else cur.angs = 0

				ji = (where(Streq(stem,'#K',2),nji))[0]
				if nji eq 0 then begin
					ji = (where(Streq(stem,'#I',2),nji))[0]
					lloc = [2,3,4,5]
				endif else lloc = [2,8,6,7]
				if nji eq 1 and ji gt jl and ji lt jend[i] then begin
					cur.lindet = 1
					dum = Strparse_mm(stem[ji],' 	',lis)
					if dum ge 2 then begin
						if not (keyword_set(olp) and cur.chan eq 0) then begin
							lis = lis[lloc]
							cur.chan = long(lis[0])
							cur.ldist = float(lis[1])
							cur.lmpc = float(lis[2])
							cur.lns = float(lis[3])
						endif else cur.chan = long(lis[2])
					endif
					chan = (where(Streq(stem[ji+1:jend[i]],''),nch))[0]
					if nch eq 0 then chan = jend[i] - ji
					achan = (where(Streq(stem[ji+1:ji+(chan>1)],'#',1),nch))[0]
					if nch ne 0 then chan = achan
					cur.chro = chan
					chan = chan*((Strparse_mm(stem[ji+1],' 	') + 1) > 1)
					if chan gt 0 then begin
						if cur.chan ne chan and $
						cur.chan ne 0 then $
						cur.lindet = 3
						cur.chan = chan
						cur.ptr[3] = ptem[ji+1]
						cur.lptr[3] = ji+1
					endif else cur.lindet = 2
				endif else cur.chan = 0

				jj = where(Streq(stem,'#J',2),njj)
				if njj ge 1 and jj[0] gt jl and jj[(njj-1)>0] lt jend[i] $
				then begin
					fildat.mcafl = 1
					cur.mcnum = njj
					if njj eq cur.ncr[1] then cur.mcstat= 1 else cur.mcstat= 3
					check = strtrim(stem[jj[0]+1],2)
					if strlen(check) gt 0 then begin
						if Streq(strmid(check,0,1),'#') then jjoff = jj + 2 $
						else jjoff = jj + 1
					endif else jjoff = jj + 2
					cur.mcptr[0:njj-1] = ptem[jjoff]
					cur.mclptr[0:njj-1] = jjoff
					for k = 0l, njj-1 do begin
						dum = Strparse_mm(stem[jj[k]],' 	',lis)
						if k eq 0 then begin
							cur.mchan = lis[2]
							check = stem[jjoff[0]]
							if Streq(' ',strcompress(check)) then $
							cur.mcstat = 2 else $
							cur.mchro = cur.mchan/(Strparse_mm(check,' 	') + 1)
							if dum ge 7 then begin
								cur.mcslp = lis[6]
								cur.mcint = lis[7]
							endif else begin
								pind = Strmatch_mm('calib_slp',$
								fildat.pp_nam[0:fildat.npp-1])
								if pind ge 0 then $
								cur.mcslp = cur.pp_val[pind]
								pind = Strmatch_mm('calib_int',$
								fildat.pp_nam[0:fildat.npp-1])
								if pind ge 0 then $
								cur.mcint = cur.pp_val[pind]
							endelse
						endif
						cur.mctim[*,k] = lis[3:4]
					endfor
				endif

				jb = where(Streq(stem,'#B',2),njb)
				if njb ge 1 and jb[(njb-1)>0] lt jl then begin
					dum = Strparse_mm(stem[jb[0]],' 	',lis)
					tnam = lis[dum]
					dum = Strparse_mm(tnam,'\/',lis)
					if njb gt 1 then begin
						bdum = Strparse_mm(stem[jb[1]],' 	',blis)
						dtt = fix(strmid(blis[0],2))
					endif else begin
						bdum = -1
						dtt = 1
					endelse
					if not fildat.pdfl then begin
						stf = ['','pilatus','apex','camera']
						sts = ['',' Pilatus','n Apex',' Camera']
						if Streq(strmid(fildat.name,0,1),dsep) then psep= dsep $
						else psep = ''
						ddum = Strparse_mm(fildat.name,dsep,llis)
						clis = [llis[0:ddum-1],stf[dtt],lis[dum]]
						tnam = psep + strjoin(clis,dsep)
						fdum = file_search(tnam)
						if fdum eq '' then begin
							clis = [llis[0:ddum-1],lis[dum]]
							tnam = psep + strjoin(clis,dsep)
							fdum = file_search(tnam)
						endif
						nam = Fnamparse(tnam,path=pat,ext=ext)
						if fdum eq '' then pat= File_get(path=pat,/inter,/dir,$
							title='Please select a' + sts[dtt] + ' data folder')
						fildat.pdpath = pat
					endif else nam = Fnamparse(strjoin(lis,dsep),ext=ext)
					if fildat.pdfl ne (2*dtt + 1) then begin
						fildat.pdfl = 2*dtt + 1
						pdini = 1
					endif else pdini = 0
					cur.pdfnam = nam
					cur.pdfext = ext
					case dtt of
						1:	begin
								if pdini then begin
									fildat.pddim = [487,195]
									fildat.pdpix = 0.172
								endif
							end
						2:	begin
								if pdini or acfl or fildat.pdpix eq 0 then begin
									if cur.ncr[1] gt 0 then begin
										cnam = $
										fildat.pdpath + cur.pdfnam + cur.pdfext
										chfile = (file_search(cnam))[0]
										if chfile ne '' then begin
											dum = Read_BIS($
											chfile,/nodata,dim=dim,pix=pix)
											fildat.pddim = dim
											fildat.pdpix = pix
										endif else begin
											print, string([13b,10b]) + cnam + $
											' is missing!'
											fildat.pddim = [0,0]
											fildat.pdpix = 0
										endelse
									endif
								endif
							end
						3:	begin
								if pdini or acfl or fildat.pdpix eq 0 then begin
									if cur.ncr[1] gt 0 then begin
										cnam = $
										fildat.pdpath + cur.pdfnam + cur.pdfext
										chfile = (file_search(cnam))[0]
										if chfile ne '' then begin
											dum = Rimg_mm(chfile,dim=dim)
											fildat.pddim = dim
											fildat.pdpix = 1
										endif else begin
											print, string([13b,10b]) + cnam + $
											' is missing!'
											fildat.pddim = [0,0]
											fildat.pdpix = 0
										endelse
									endif
								endif
							end
					endcase
					cur.pddim = fildat.pddim
					cur.pdpix = fildat.pdpix
					if bdum ge 4 then begin
						cur.pdxc = blis[1]
						cur.pdyc = blis[2]
						cur.pdist = blis[3]
						cur.pdhv = 1
						if Streq(blis[bdum],'H',1) then cur.pdhv = 0
						if bdum eq 4 then begin
							if max(byte(blis[bdum])) lt 58 $
							then cur.pdsdist = blis[bdum]
						endif else cur.pdsdist = blis[4]
						if cur.pdist lt cur.pdsdist then begin
							tdist = cur.pdist
							cur.pdist = cur.pdsdist
							cur.pdsdist = tdist
						endif
						if cur.pdist gt pdfar then cur.pdfar = 1
						cur.pdstat = 2*dtt + 1
					endif else cur.pdstat = 1
					if cur.ncr[1] eq 0 then cur.pdstat = 2
				endif

				if (cur.mcstat eq 1 or cur.lindet eq 1) and cur.stat eq 3 $
				then cur.stat = 1

				if check_math(mask=128) ne 0 then begin
					cur.stat = 7
					print
					print,'		' + strcompress( $
					'Illegal numeric data in scan #' + string(isc) + ' !!!')
				endif
			endif
			fildat.scan[isc] = cur
		endfor
	endif

	if keyword_set(dtl) then begin
		print
		print, fildat.name
		if fildat.status ne 1 then begin
			print
			print, warn[fildat.status]
		endif
	end

	return
end