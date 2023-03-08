Function User_data_old, experiments = exps, correct = cor

;+
; NAME:
;		USER_DATA
; VERSION:
;		8.334
; PURPOSE:
;		Reads user data from EXCEL generated text file.
; CATEGORY:
;		I/O
; CALLING SEQUENCE:
;		Result = USER_DATA([, EXPERIMENTS = EXP])
; INPUTS:
;		None.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	EXPERIMENTS
;		Optional output, see below.
;	/CORRECT
;		Switch, causes correction data to be read from the file CORR_LIST.DAT
;		in the DATA folder.  Set by default, disabled if explicitly set to 0.
; OUTPUTS:
; 		Returns the read user data in a structure containing:
; 			NREC	:	Number of user records.
; 			UREC	:	An array of NREC structures of type USER.  See 
; 						USER__DEFINE for details.
; OPTIONAL OUTPUT PARAMETERS:
;	EXPERIMENTS
;		Returns the experiments data pertaining to the user data, in a structure
;		containing:
;			NEXP	:	Number of experiment records.
;			EREC	:	An array of NEXP structures of type EXPERIMENT.  See
;						EXPERIMENT__DEFINE for details.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
; 		Reads the tab separated text file created by EXCEL and generates the
; 		needed structure(s).  Calls RASC_TABS.  Calls LEXISORT, SORPURGE, STREQ,
; 		STRPARSE_MM and STRPARSE_MM, from MIDL. 
; MODIFICATION HISTORY:
;		Created 15-JAN-2014 by Mati Meron.
;		Modified 20-MAY-2014 by Mati Meron.  Internal changes.
;		Modified 20-SEP-2014 by Mati Meron.  Internal changes.
;		Modified 1-NOV-2014 by Mati Meron.  Internal changes.
;		Modified 15-NOV-2014 by Mati Meron.  Added keyword CORRECT.
;-

	on_error, 1

	hnam = ['Experiment ID','Posted Date','Badge No','First Name','Last Name',$
		'User Type', 'Gender', 'Institution', 'Institutional Role', $
		'Organization Type', 'Funding Source', 'Spokesperson Flag', $
		'Subject', 'Technique Name', 'Pen']
	uind = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14]
	spocom = ['Graber','Meron']

	udat = Rasc_tabs(head=head)
	nrec = (size(udat))[2]
	users = {nrec: nrec, urec: replicate({user},nrec)}
	for i = 0, n_elements(hnam) - 1 do begin
		loc = where(Streq(head,hnam[i]),nloc)
		if nloc ne 0 then users.urec.(uind[i]) = reform(udat[loc,*])
	endfor

	date = users.urec.date
	mdy = lonarr(3,nrec)
	for j = 0, nrec-1 do begin
		dum = Strparse_mm(date[j],'/',lis)
		if dum eq 2 then mdy[*,j] = lis
	endfor
	users.urec.month = reform(mdy[0,*])
	users.urec.day = reform(mdy[1,*])
	users.urec.year = reform(mdy[2,*])
	users.urec.run = (users.urec.month+3)/4
	dum = where(strmid(users.urec.fname,0,1) eq '"',ndum)
	for i = 0, ndum-1 do users.urec[dum[i]].fname = $
		Strsub_mm(users.urec[dum[i]].fname,'',wild='"',/rec)
	dum = where(strmid(users.urec.lname,0,1) eq '"',ndum)
	for i = 0, ndum-1 do users.urec[dum[i]].lname = $
		Strsub_mm(users.urec[dum[i]].lname,'',wild='"',/rec)
	sord = Lexisort(users.urec.expid,users.urec.lname,users.urec.fname)
	users.urec = users.urec[sord]

	sex = Sorpurge(users.urec.expid,net=nexp)
	exps = {nexp: nexp, erec: replicate({experiment},nexp)}
	exps.erec.expid = users.urec[sex].expid
	exps.erec.pen = users.urec[sex].pen
	exps.erec.run = users.urec[sex].run

	csta = strarr(nexp)
	if Default(cor,1,/dtyp) ne 0 then begin
		Load_corr, expid = xid, station = stat
		for i = 0, n_elements(xid)- 1 do begin
			dum = where(exps.erec.expid eq xid[i], ndum)
			if ndum gt 0 then csta[dum] = stat[i] else $
			message, 'Expid ' + string(xid[i],form='(i0)') + ' not found!', /con
		endfor
	endif

	for i = 0, nexp-1 do begin
		cur = exps.erec[i]
		pmess =  ' discrepancy in exp. ' + string(cur.expid,form='(i0)')
		whi = where(users.urec.expid eq cur.expid)
		ucur = users.urec[whi]
		sda = Sorpurge(ucur.date,net=nsd)
		if nsd gt 1 then begin
			message, 'Date' + pmess, /con
			ind = Lexisort(ucur[sda].year,ucur[sda].month,ucur[sda].day)
			sda = sda[ind[0]]
		endif
		cur.date = ucur[sda].date
		cur.month = ucur[sda].month
		cur.day = ucur[sda].day
		cur.year = ucur[sda].year
		names = ucur.fname + ' ' + ucur.lname
		wsp = where(Streq(ucur.spoksp,'y'),nsp)
		if nsp gt 0 then begin
			spnams = names[wsp]
			ssp = Sorpurge(spnams,net=nss)
			if nss gt 1 then message, 'Spokesperson' + pmess, /con
			cur.spokfn = ucur[wsp[0]].fname
			cur.spokln = ucur[wsp[0]].lname
			cur.fund = ucur[wsp[0]].fund
		endif else cur.spokfn = (cur.spokln = (cur.fund = '?'))
		sna = Sorpurge(names,net=nsn)
		cur.nnames = nsn
		names = names[sna]
		ord = sort(ucur[sna].lname)
		cur.names[0:nsn-1] = names[ord]
		if csta[i] eq '' then begin
			dum = Strparse_mm(cur.pen,'-',lis)
			if dum ge 1 then begin
				sta = strmid(lis[1],2)
				if strlen(sta) gt 1 then begin
					if Strmatch_mm(cur.spokln,spocom) ge 0 then sta = 'A' $
					else sta = '?'
				endif else if sta eq '' or sta eq ' ' then sta = '?'
			endif else sta = '?'
		endif else sta = csta[i]
		cur.station = sta
		users.urec[whi].station = sta
		sdi = Sorpurge(ucur.discip,net=nsd)
		if ucur[sdi[0]].discip eq '' then begin
			nsd = nsd-1
			if nsd gt 0 then sdi = sdi[1:*]
		endif
		if nsd gt 0 then begin 
			cur.ndiscip = nsd
			cur.discip[0:nsd-1] = ucur[sdi].discip
		endif
		ste = Sorpurge(ucur.tech,net=nst)
		if ucur[ste[0]].tech eq '' then begin
			nst = nst-1
			if nst gt 0 then ste = ste[1:*]
		endif
		if nst gt 0 then begin
			cur.ntech = nst
			cur.tech[0:nst-1] = ucur[ste].tech
		endif
		exps.erec[i] = cur
	endfor
	
	return, users
end