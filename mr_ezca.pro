dum = strparse_mm(!path,',:'+sdep(/ds),lis)
if strmatch_mm('idl_user',lis) lt 0 then stop
@idl_startup