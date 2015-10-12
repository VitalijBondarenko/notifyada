------------------------------------------------------------------------------
--                                                                          --
-- Copyright (c) 2013-2015 Vitalij Bondarenko <vibondare@gmail.com>         --
--                                                                          --
------------------------------------------------------------------------------
--                                                                          --
-- The MIT License (MIT)                                                    --
--                                                                          --
-- Permission is hereby granted, free of charge, to any person obtaining a  --
-- copy of this software and associated documentation files (the            --
-- "Software"), to deal in the Software without restriction, including      --
-- without limitation the rights to use, copy, modify, merge, publish,      --
-- distribute, sublicense, and/or sell copies of the Software, and to       --
-- permit persons to whom the Software is furnished to do so, subject to    --
-- the following conditions:                                                --
--                                                                          --
-- The above copyright notice and this permission notice shall be included  --
-- in all copies or substantial portions of the Software.                   --
--                                                                          --
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS  --
-- OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF               --
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.   --
-- IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY     --
-- CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,     --
-- TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE        --
-- SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                   --
------------------------------------------------------------------------------

with Interfaces.C.Strings; use Interfaces.C.Strings;
with System;               use System;

package body Notify is

   ----------
   -- Init --
   ----------

   function Init (App_Name : UTF8_String) return Boolean is
      function Internal (App_Name : UTF8_String) return Gboolean;
      pragma Import (C, Internal, "notify_init");

   begin
      return 0 /= Internal (App_Name & ASCII.NUL);
   end Init;

   ------------
   -- Uninit --
   ------------

   procedure Uninit is
      procedure Internal;
      pragma Import (C, Internal, "notify_uninit");

   begin
      Internal;
   end Uninit;

   ----------------
   -- Is_Initted --
   ----------------

   function Is_Initted return Boolean is
      function Internal return Gboolean;
      pragma Import (C, Internal, "notify_is_initted");

   begin
      return 0 /= Internal;
   end Is_Initted;

   ------------------
   -- Get_App_Name --
   ------------------

   function Get_App_Name return UTF8_String is
      function Internal return chars_ptr;
      pragma Import (C, Internal, "notify_get_app_name");

      Name : chars_ptr := Internal;

   begin
      if Name = Null_Ptr then
         return "";
      else
         declare N : UTF8_String := Value (Name);
         begin
            Free (Name);
            return N;
         end;
      end if;
   end Get_App_Name;

   ------------------
   -- Set_App_Name --
   ------------------

   procedure Set_App_Name (App_Name : UTF8_String) is
      procedure Internal (App_Name : UTF8_String);
      pragma Import (C, Internal, "notify_set_app_name");

   begin
      Internal (App_Name & ASCII.NUL);
   end Set_App_Name;

   ---------------------
   -- Get_Server_Caps --
   ---------------------

   function Get_Server_Caps return Gtk.Enums.String_List.Glist is
      function Internal return System.Address;
      pragma Import (C, Internal, "notify_get_server_caps");

      List : Gtk.Enums.String_List.Glist;

   begin
      String_List.Set_Object (List, Internal);
      return List;
   end Get_Server_Caps;

   ---------------------
   -- Get_Server_Info --
   ---------------------

   function Get_Server_Info
     (Name         : out String_Ptr;
      Vendor       : out String_Ptr;
      Version      : out String_Ptr;
      Spec_Version : out String_Ptr) return Boolean
   is
      function Internal
        (Name         : access chars_ptr;
         Vendor       : access chars_ptr;
         Version      : access chars_ptr;
         Spec_Version : access chars_ptr)
         return Gboolean;
      pragma Import (C, internal, "notify_get_server_info");

      R    : Boolean;
      N    : aliased chars_ptr;
      Ven  : aliased chars_ptr;
      Ver  : aliased chars_ptr;
      Spec : aliased chars_ptr;

   begin
      R := 0 /= Internal (N'Access, Ven'Access, Ver'Access, Spec'Access);

      if R then
         Name := new String'(Value (N));
         Vendor :=  new String'(Value (Ven));
         Version :=  new String'(Value (Ver));
         Spec_Version :=  new String'(Value (Spec));
      end if;

      return R;
   end Get_Server_Info;

end Notify;
