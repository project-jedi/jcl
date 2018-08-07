type
   // Dummy opaque record
   _locale_data = Record
   end;
   Plocale_data = ^_locale_data;

   TLocaleData =  _locale_data;
   PLocaleData = ^TLocaleData;

   P__locale_struct = ^__locale_struct;
   __locale_struct = record
        __locales : array[0..12] of Plocale_data;
        __ctype_b : Pword;
        __ctype_tolower : Plongint;
        __ctype_toupper : Plongint;
     end;
   __locale_t = P__locale_struct;
   P__locale_t = ^__locale_t;

   TLocale = __locale_struct;
   PLocale = ^TLocale;
