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

   -----------------
   -- Notify_Init --
   -----------------

   function Notify_Init (App_Name : UTF8_String) return Boolean is
      function Internal (App_Name : chars_ptr) return Gboolean;
      pragma Import (C, Internal, "notify_init");
   begin
      return 0 /= Internal (New_String (App_Name));
   end Notify_Init;

   -----------------
   -- Notify_Init --
   -----------------

   procedure Notify_Init (App_Name : UTF8_String) is
      Success : Boolean;
   begin
      Success := Notify_Init (App_Name);
   end Notify_Init;

   -------------------
   -- Notify_Uninit --
   -------------------

   procedure Notify_Uninit is
      procedure Internal;
      pragma Import (C, Internal, "notify_uninit");
   begin
      Internal;
   end Notify_Uninit;

   -----------------------
   -- Notify_Is_Initted --
   -----------------------

   function Notify_Is_Initted return Boolean is
      function Internal return Gboolean;
      pragma Import (C, Internal, "notify_is_initted");
   begin
      return 0 /= Internal;
   end Notify_Is_Initted;

   -------------------------
   -- Notify_Get_App_Name --
   -------------------------

   function Notify_Get_App_Name return UTF8_String is
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
   end Notify_Get_App_Name;

   -------------------------
   -- Notify_Set_App_Name --
   -------------------------

   procedure Notify_Set_App_Name (App_Name : UTF8_String) is
      procedure Internal (App_Name : chars_ptr);
      pragma Import (C, Internal, "notify_set_app_name");
   begin
      Internal (New_String (App_Name));
   end Notify_Set_App_Name;

   ----------------------------
   -- Notify_Get_Server_Caps --
   ----------------------------

   function Notify_Get_Server_Caps return Gtk.Enums.String_List.Glist is
      function Internal return System.Address;
      pragma Import (C, Internal, "notify_get_server_caps");

      List : Gtk.Enums.String_List.Glist;
   begin
      String_List.Set_Object (List, Internal);
      return List;
   end Notify_Get_Server_Caps;

   ----------------------------
   -- Notify_Get_Server_Info --
   ----------------------------

   function Notify_Get_Server_Info
     (Name         : out String_Ptr;
      Vendor       : out String_Ptr;
      Version      : out String_Ptr;
      Spec_Version : out String_Ptr) return Boolean
   is
      function Internal
        (Name         : access chars_ptr;
         Vendor       : access chars_ptr;
         Version      : access chars_ptr;
         Spec_Version : access chars_ptr) return Gboolean;
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
   end Notify_Get_Server_Info;

end Notify;
