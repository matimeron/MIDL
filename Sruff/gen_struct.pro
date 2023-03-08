Function Gen_struct, dname, ptype

    on_error, 1

    pu = {proc_beam, rawdat: string(Default(dname,'',/dtype),format='(a32)'), $
	source: Un_struct(nhar_max = nhmx), proc: Pr_struct(ptype), $
	dsets: 0l, dvals: fltarr(nhmx)}

    return, pu
end
