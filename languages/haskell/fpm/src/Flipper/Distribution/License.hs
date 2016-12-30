{-|
Module      : Flipper.Distribution.License
Description : Flipper Package Licenses
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

This module provides a data type for representing software licenses. Any license
may be used with Flipper packages, but FPM provides the full text for some of
the most common licenses for convenience.

= Disclaimer

The descriptions of software licenses provided by this documentation are
intended for informational purposes only and in no way constitute legal advice.
Please read the text of the licenses and consult a lawyer for any advice
regarding software licensing.
-}

{-# LANGUAGE DeriveAnyClass
           , DeriveDataTypeable
           , DeriveGeneric
           , OverloadedStrings
           #-}

module Flipper.Distribution.License (
    License(..)
  , licenseText
  , parseLicense
  ) where

import Control.DeepSeq

import Data.Binary

import Data.Data

import qualified Data.Text as T

import Flipper.Distribution.Parser

import GHC.Generics

import qualified Text.Megaparsec      as M
import qualified Text.Megaparsec.Text as M

-- | Licenses for source code release.
data License =
    -- | GNU General Public License,
    --   <https://www.gnu.org/licenses/old-licenses/gpl-2.0.html version 2>
    GPL2
    -- | GNU General Public License,
    --   <https://www.gnu.org/licenses/gpl.html version 3>
  | GPL3
    -- | GNU Affero General Public License,
    --   <https://www.gnu.org/licenses/agpl.html version 3>
  | AGPL
    -- | GNU Lesser General Public License,
    --   <https://www.gnu.org/licenses/old-licenses/lgpl-2.1.html version 2.1>
  | LGPL21
    -- | GNU Lesser General Public License,
    --   <https://www.gnu.org/licenses/lgpl.html version 3>
  | LGPL3
    -- | BSD License,
    --   <http://www.opensource.org/licenses/bsd-license 2-clause>
  | BSD2
    -- | BSD License,
    --   <http://www.opensource.org/licenses/bsd-3-clause 3-clause>
  | BSD3
    -- | <http://www.opensource.org/licenses/MIT MIT License>
  | MIT
    -- | Mozilla Public License,
    --   <https://www.mozilla.org/MPL/ version 2.0>
  | MPL2
    -- | Apache License,
    --   <https://www.apache.org/licenses/ version 2.0>
  | Apache2
    -- | The author of a package disclaims any copyright to its source code and
    --   dedicates it to the public domain. This is not a software license.
    --   Please note that it is not possible to dedicate works to the public
    --   domain in every jurisdiction, nor is a work that is in the public
    --   domain in one jurisdiction necessarily in the public domain elsewhere.
  | PublicDomain
    -- | Explicitly 'All Rights Reserved', eg for proprietary software. The
    --   package may not be legally modified or redistributed by anyone but the
    --   rightsholder.
  | AllRightsReserved
    -- | Any other software license.
  | Other
  deriving ( Eq
           , Ord
           , Show
           , Enum
           , Data
           , Typeable
           , Generic
           , NFData
           , Binary
           )

-- | A license's body text, for automatically generating a LICENSE file.
licenseText :: License -> T.Text
licenseText GPL2              ="                    GNU GENERAL PUBLIC LICENSE\n\
                               \                       Version 2, June 1991\n\
                               \\n\
                               \ Copyright (C) 1989, 1991 Free Software Foundation, Inc.,\n\
                               \ 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA\n\
                               \ Everyone is permitted to copy and distribute verbatim copies\n\
                               \ of this license document, but changing it is not allowed.\n\
                               \\n\
                               \                            Preamble\n\
                               \\n\
                               \  The licenses for most software are designed to take away your\n\
                               \freedom to share and change it.  By contrast, the GNU General Public\n\
                               \License is intended to guarantee your freedom to share and change free\n\
                               \software--to make sure the software is free for all its users.  This\n\
                               \General Public License applies to most of the Free Software\n\
                               \Foundation's software and to any other program whose authors commit to\n\
                               \using it.  (Some other Free Software Foundation software is covered by\n\
                               \the GNU Lesser General Public License instead.)  You can apply it to\n\
                               \your programs, too.\n\
                               \\n\
                               \  When we speak of free software, we are referring to freedom, not\n\
                               \price.  Our General Public Licenses are designed to make sure that you\n\
                               \have the freedom to distribute copies of free software (and charge for\n\
                               \this service if you wish), that you receive source code or can get it\n\
                               \if you want it, that you can change the software or use pieces of it\n\
                               \in new free programs; and that you know you can do these things.\n\
                               \\n\
                               \  To protect your rights, we need to make restrictions that forbid\n\
                               \anyone to deny you these rights or to ask you to surrender the rights.\n\
                               \These restrictions translate to certain responsibilities for you if you\n\
                               \distribute copies of the software, or if you modify it.\n\
                               \\n\
                               \  For example, if you distribute copies of such a program, whether\n\
                               \gratis or for a fee, you must give the recipients all the rights that\n\
                               \you have.  You must make sure that they, too, receive or can get the\n\
                               \source code.  And you must show them these terms so they know their\n\
                               \rights.\n\
                               \\n\
                               \  We protect your rights with two steps: (1) copyright the software, and\n\
                               \(2) offer you this license which gives you legal permission to copy,\n\
                               \distribute and/or modify the software.\n\
                               \\n\
                               \  Also, for each author's protection and ours, we want to make certain\n\
                               \that everyone understands that there is no warranty for this free\n\
                               \software.  If the software is modified by someone else and passed on, we\n\
                               \want its recipients to know that what they have is not the original, so\n\
                               \that any problems introduced by others will not reflect on the original\n\
                               \authors' reputations.\n\
                               \\n\
                               \  Finally, any free program is threatened constantly by software\n\
                               \patents.  We wish to avoid the danger that redistributors of a free\n\
                               \program will individually obtain patent licenses, in effect making the\n\
                               \program proprietary.  To prevent this, we have made it clear that any\n\
                               \patent must be licensed for everyone's free use or not licensed at all.\n\
                               \\n\
                               \  The precise terms and conditions for copying, distribution and\n\
                               \modification follow.\n\
                               \\n\
                               \                    GNU GENERAL PUBLIC LICENSE\n\
                               \   TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION\n\
                               \\n\
                               \  0. This License applies to any program or other work which contains\n\
                               \a notice placed by the copyright holder saying it may be distributed\n\
                               \under the terms of this General Public License.  The \"Program\", below,\n\
                               \refers to any such program or work, and a \"work based on the Program\"\n\
                               \means either the Program or any derivative work under copyright law:\n\
                               \that is to say, a work containing the Program or a portion of it,\n\
                               \either verbatim or with modifications and/or translated into another\n\
                               \language.  (Hereinafter, translation is included without limitation in\n\
                               \the term \"modification\".)  Each licensee is addressed as \"you\".\n\
                               \\n\
                               \Activities other than copying, distribution and modification are not\n\
                               \covered by this License; they are outside its scope.  The act of\n\
                               \running the Program is not restricted, and the output from the Program\n\
                               \is covered only if its contents constitute a work based on the\n\
                               \Program (independent of having been made by running the Program).\n\
                               \Whether that is true depends on what the Program does.\n\
                               \\n\
                               \  1. You may copy and distribute verbatim copies of the Program's\n\
                               \source code as you receive it, in any medium, provided that you\n\
                               \conspicuously and appropriately publish on each copy an appropriate\n\
                               \copyright notice and disclaimer of warranty; keep intact all the\n\
                               \notices that refer to this License and to the absence of any warranty;\n\
                               \and give any other recipients of the Program a copy of this License\n\
                               \along with the Program.\n\
                               \\n\
                               \You may charge a fee for the physical act of transferring a copy, and\n\
                               \you may at your option offer warranty protection in exchange for a fee.\n\
                               \\n\
                               \  2. You may modify your copy or copies of the Program or any portion\n\
                               \of it, thus forming a work based on the Program, and copy and\n\
                               \distribute such modifications or work under the terms of Section 1\n\
                               \above, provided that you also meet all of these conditions:\n\
                               \\n\
                               \    a) You must cause the modified files to carry prominent notices\n\
                               \    stating that you changed the files and the date of any change.\n\
                               \\n\
                               \    b) You must cause any work that you distribute or publish, that in\n\
                               \    whole or in part contains or is derived from the Program or any\n\
                               \    part thereof, to be licensed as a whole at no charge to all third\n\
                               \    parties under the terms of this License.\n\
                               \\n\
                               \    c) If the modified program normally reads commands interactively\n\
                               \    when run, you must cause it, when started running for such\n\
                               \    interactive use in the most ordinary way, to print or display an\n\
                               \    announcement including an appropriate copyright notice and a\n\
                               \    notice that there is no warranty (or else, saying that you provide\n\
                               \    a warranty) and that users may redistribute the program under\n\
                               \    these conditions, and telling the user how to view a copy of this\n\
                               \    License.  (Exception: if the Program itself is interactive but\n\
                               \    does not normally print such an announcement, your work based on\n\
                               \    the Program is not required to print an announcement.)\n\
                               \\n\
                               \These requirements apply to the modified work as a whole.  If\n\
                               \identifiable sections of that work are not derived from the Program,\n\
                               \and can be reasonably considered independent and separate works in\n\
                               \themselves, then this License, and its terms, do not apply to those\n\
                               \sections when you distribute them as separate works.  But when you\n\
                               \distribute the same sections as part of a whole which is a work based\n\
                               \on the Program, the distribution of the whole must be on the terms of\n\
                               \this License, whose permissions for other licensees extend to the\n\
                               \entire whole, and thus to each and every part regardless of who wrote it.\n\
                               \\n\
                               \Thus, it is not the intent of this section to claim rights or contest\n\
                               \your rights to work written entirely by you; rather, the intent is to\n\
                               \exercise the right to control the distribution of derivative or\n\
                               \collective works based on the Program.\n\
                               \\n\
                               \In addition, mere aggregation of another work not based on the Program\n\
                               \with the Program (or with a work based on the Program) on a volume of\n\
                               \a storage or distribution medium does not bring the other work under\n\
                               \the scope of this License.\n\
                               \\n\
                               \  3. You may copy and distribute the Program (or a work based on it,\n\
                               \under Section 2) in object code or executable form under the terms of\n\
                               \Sections 1 and 2 above provided that you also do one of the following:\n\
                               \\n\
                               \    a) Accompany it with the complete corresponding machine-readable\n\
                               \    source code, which must be distributed under the terms of Sections\n\
                               \    1 and 2 above on a medium customarily used for software interchange; or,\n\
                               \\n\
                               \    b) Accompany it with a written offer, valid for at least three\n\
                               \    years, to give any third party, for a charge no more than your\n\
                               \    cost of physically performing source distribution, a complete\n\
                               \    machine-readable copy of the corresponding source code, to be\n\
                               \    distributed under the terms of Sections 1 and 2 above on a medium\n\
                               \    customarily used for software interchange; or,\n\
                               \\n\
                               \    c) Accompany it with the information you received as to the offer\n\
                               \    to distribute corresponding source code.  (This alternative is\n\
                               \    allowed only for noncommercial distribution and only if you\n\
                               \    received the program in object code or executable form with such\n\
                               \    an offer, in accord with Subsection b above.)\n\
                               \\n\
                               \The source code for a work means the preferred form of the work for\n\
                               \making modifications to it.  For an executable work, complete source\n\
                               \code means all the source code for all modules it contains, plus any\n\
                               \associated interface definition files, plus the scripts used to\n\
                               \control compilation and installation of the executable.  However, as a\n\
                               \special exception, the source code distributed need not include\n\
                               \anything that is normally distributed (in either source or binary\n\
                               \form) with the major components (compiler, kernel, and so on) of the\n\
                               \operating system on which the executable runs, unless that component\n\
                               \itself accompanies the executable.\n\
                               \\n\
                               \If distribution of executable or object code is made by offering\n\
                               \access to copy from a designated place, then offering equivalent\n\
                               \access to copy the source code from the same place counts as\n\
                               \distribution of the source code, even though third parties are not\n\
                               \compelled to copy the source along with the object code.\n\
                               \\n\
                               \  4. You may not copy, modify, sublicense, or distribute the Program\n\
                               \except as expressly provided under this License.  Any attempt\n\
                               \otherwise to copy, modify, sublicense or distribute the Program is\n\
                               \void, and will automatically terminate your rights under this License.\n\
                               \However, parties who have received copies, or rights, from you under\n\
                               \this License will not have their licenses terminated so long as such\n\
                               \parties remain in full compliance.\n\
                               \\n\
                               \  5. You are not required to accept this License, since you have not\n\
                               \signed it.  However, nothing else grants you permission to modify or\n\
                               \distribute the Program or its derivative works.  These actions are\n\
                               \prohibited by law if you do not accept this License.  Therefore, by\n\
                               \modifying or distributing the Program (or any work based on the\n\
                               \Program), you indicate your acceptance of this License to do so, and\n\
                               \all its terms and conditions for copying, distributing or modifying\n\
                               \the Program or works based on it.\n\
                               \\n\
                               \  6. Each time you redistribute the Program (or any work based on the\n\
                               \Program), the recipient automatically receives a license from the\n\
                               \original licensor to copy, distribute or modify the Program subject to\n\
                               \these terms and conditions.  You may not impose any further\n\
                               \restrictions on the recipients' exercise of the rights granted herein.\n\
                               \You are not responsible for enforcing compliance by third parties to\n\
                               \this License.\n\
                               \\n\
                               \  7. If, as a consequence of a court judgment or allegation of patent\n\
                               \infringement or for any other reason (not limited to patent issues),\n\
                               \conditions are imposed on you (whether by court order, agreement or\n\
                               \otherwise) that contradict the conditions of this License, they do not\n\
                               \excuse you from the conditions of this License.  If you cannot\n\
                               \distribute so as to satisfy simultaneously your obligations under this\n\
                               \License and any other pertinent obligations, then as a consequence you\n\
                               \may not distribute the Program at all.  For example, if a patent\n\
                               \license would not permit royalty-free redistribution of the Program by\n\
                               \all those who receive copies directly or indirectly through you, then\n\
                               \the only way you could satisfy both it and this License would be to\n\
                               \refrain entirely from distribution of the Program.\n\
                               \\n\
                               \If any portion of this section is held invalid or unenforceable under\n\
                               \any particular circumstance, the balance of the section is intended to\n\
                               \apply and the section as a whole is intended to apply in other\n\
                               \circumstances.\n\
                               \\n\
                               \It is not the purpose of this section to induce you to infringe any\n\
                               \patents or other property right claims or to contest validity of any\n\
                               \such claims; this section has the sole purpose of protecting the\n\
                               \integrity of the free software distribution system, which is\n\
                               \implemented by public license practices.  Many people have made\n\
                               \generous contributions to the wide range of software distributed\n\
                               \through that system in reliance on consistent application of that\n\
                               \system; it is up to the author/donor to decide if he or she is willing\n\
                               \to distribute software through any other system and a licensee cannot\n\
                               \impose that choice.\n\
                               \\n\
                               \This section is intended to make thoroughly clear what is believed to\n\
                               \be a consequence of the rest of this License.\n\
                               \\n\
                               \  8. If the distribution and/or use of the Program is restricted in\n\
                               \certain countries either by patents or by copyrighted interfaces, the\n\
                               \original copyright holder who places the Program under this License\n\
                               \may add an explicit geographical distribution limitation excluding\n\
                               \those countries, so that distribution is permitted only in or among\n\
                               \countries not thus excluded.  In such case, this License incorporates\n\
                               \the limitation as if written in the body of this License.\n\
                               \\n\
                               \  9. The Free Software Foundation may publish revised and/or new versions\n\
                               \of the General Public License from time to time.  Such new versions will\n\
                               \be similar in spirit to the present version, but may differ in detail to\n\
                               \address new problems or concerns.\n\
                               \\n\
                               \Each version is given a distinguishing version number.  If the Program\n\
                               \specifies a version number of this License which applies to it and \"any\n\
                               \later version\", you have the option of following the terms and conditions\n\
                               \either of that version or of any later version published by the Free\n\
                               \Software Foundation.  If the Program does not specify a version number of\n\
                               \this License, you may choose any version ever published by the Free Software\n\
                               \Foundation.\n\
                               \\n\
                               \  10. If you wish to incorporate parts of the Program into other free\n\
                               \programs whose distribution conditions are different, write to the author\n\
                               \to ask for permission.  For software which is copyrighted by the Free\n\
                               \Software Foundation, write to the Free Software Foundation; we sometimes\n\
                               \make exceptions for this.  Our decision will be guided by the two goals\n\
                               \of preserving the free status of all derivatives of our free software and\n\
                               \of promoting the sharing and reuse of software generally.\n\
                               \\n\
                               \                            NO WARRANTY\n\
                               \\n\
                               \  11. BECAUSE THE PROGRAM IS LICENSED FREE OF CHARGE, THERE IS NO WARRANTY\n\
                               \FOR THE PROGRAM, TO THE EXTENT PERMITTED BY APPLICABLE LAW.  EXCEPT WHEN\n\
                               \OTHERWISE STATED IN WRITING THE COPYRIGHT HOLDERS AND/OR OTHER PARTIES\n\
                               \PROVIDE THE PROGRAM \"AS IS\" WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESSED\n\
                               \OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF\n\
                               \MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  THE ENTIRE RISK AS\n\
                               \TO THE QUALITY AND PERFORMANCE OF THE PROGRAM IS WITH YOU.  SHOULD THE\n\
                               \PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF ALL NECESSARY SERVICING,\n\
                               \REPAIR OR CORRECTION.\n\
                               \\n\
                               \  12. IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING\n\
                               \WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MAY MODIFY AND/OR\n\
                               \REDISTRIBUTE THE PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU FOR DAMAGES,\n\
                               \INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING\n\
                               \OUT OF THE USE OR INABILITY TO USE THE PROGRAM (INCLUDING BUT NOT LIMITED\n\
                               \TO LOSS OF DATA OR DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY\n\
                               \YOU OR THIRD PARTIES OR A FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER\n\
                               \PROGRAMS), EVEN IF SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE\n\
                               \POSSIBILITY OF SUCH DAMAGES.\n\
                               \\n\
                               \                     END OF TERMS AND CONDITIONS\n\
                               \\n\
                               \            How to Apply These Terms to Your New Programs\n\
                               \\n\
                               \  If you develop a new program, and you want it to be of the greatest\n\
                               \possible use to the public, the best way to achieve this is to make it\n\
                               \free software which everyone can redistribute and change under these terms.\n\
                               \\n\
                               \  To do so, attach the following notices to the program.  It is safest\n\
                               \to attach them to the start of each source file to most effectively\n\
                               \convey the exclusion of warranty; and each file should have at least\n\
                               \the \"copyright\" line and a pointer to where the full notice is found.\n\
                               \\n\
                               \    <one line to give the program's name and a brief idea of what it does.>\n\
                               \    Copyright (C) <year>  <name of author>\n\
                               \\n\
                               \    This program is free software; you can redistribute it and/or modify\n\
                               \    it under the terms of the GNU General Public License as published by\n\
                               \    the Free Software Foundation; either version 2 of the License, or\n\
                               \    (at your option) any later version.\n\
                               \\n\
                               \    This program is distributed in the hope that it will be useful,\n\
                               \    but WITHOUT ANY WARRANTY; without even the implied warranty of\n\
                               \    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n\
                               \    GNU General Public License for more details.\n\
                               \\n\
                               \    You should have received a copy of the GNU General Public License along\n\
                               \    with this program; if not, write to the Free Software Foundation, Inc.,\n\
                               \    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.\n\
                               \\n\
                               \Also add information on how to contact you by electronic and paper mail.\n\
                               \\n\
                               \If the program is interactive, make it output a short notice like this\n\
                               \when it starts in an interactive mode:\n\
                               \\n\
                               \    Gnomovision version 69, Copyright (C) year name of author\n\
                               \    Gnomovision comes with ABSOLUTELY NO WARRANTY; for details type `show w'.\n\
                               \    This is free software, and you are welcome to redistribute it\n\
                               \    under certain conditions; type `show c' for details.\n\
                               \\n\
                               \The hypothetical commands `show w' and `show c' should show the appropriate\n\
                               \parts of the General Public License.  Of course, the commands you use may\n\
                               \be called something other than `show w' and `show c'; they could even be\n\
                               \mouse-clicks or menu items--whatever suits your program.\n\
                               \\n\
                               \You should also get your employer (if you work as a programmer) or your\n\
                               \school, if any, to sign a \"copyright disclaimer\" for the program, if\n\
                               \necessary.  Here is a sample; alter the names:\n\
                               \\n\
                               \  Yoyodyne, Inc., hereby disclaims all copyright interest in the program\n\
                               \  `Gnomovision' (which makes passes at compilers) written by James Hacker.\n\
                               \\n\
                               \  <signature of Ty Coon>, 1 April 1989\n\
                               \  Ty Coon, President of Vice\n\
                               \\n\
                               \This General Public License does not permit incorporating your program into\n\
                               \proprietary programs.  If your program is a subroutine library, you may\n\
                               \consider it more useful to permit linking proprietary applications with the\n\
                               \library.  If this is what you want to do, use the GNU Lesser General\n\
                               \Public License instead of this License.\n"
licenseText GPL3              = "                    GNU GENERAL PUBLIC LICENSE\n\
                                \                       Version 3, 29 June 2007\n\
                                \\n\
                                \ Copyright (C) 2007 Free Software Foundation, Inc. <http://fsf.org/>\n\
                                \ Everyone is permitted to copy and distribute verbatim copies\n\
                                \ of this license document, but changing it is not allowed.\n\
                                \\n\
                                \                            Preamble\n\
                                \\n\
                                \  The GNU General Public License is a free, copyleft license for\n\
                                \software and other kinds of works.\n\
                                \\n\
                                \  The licenses for most software and other practical works are designed\n\
                                \to take away your freedom to share and change the works.  By contrast,\n\
                                \the GNU General Public License is intended to guarantee your freedom to\n\
                                \share and change all versions of a program--to make sure it remains free\n\
                                \software for all its users.  We, the Free Software Foundation, use the\n\
                                \GNU General Public License for most of our software; it applies also to\n\
                                \any other work released this way by its authors.  You can apply it to\n\
                                \your programs, too.\n\
                                \\n\
                                \  When we speak of free software, we are referring to freedom, not\n\
                                \price.  Our General Public Licenses are designed to make sure that you\n\
                                \have the freedom to distribute copies of free software (and charge for\n\
                                \them if you wish), that you receive source code or can get it if you\n\
                                \want it, that you can change the software or use pieces of it in new\n\
                                \free programs, and that you know you can do these things.\n\
                                \\n\
                                \  To protect your rights, we need to prevent others from denying you\n\
                                \these rights or asking you to surrender the rights.  Therefore, you have\n\
                                \certain responsibilities if you distribute copies of the software, or if\n\
                                \you modify it: responsibilities to respect the freedom of others.\n\
                                \\n\
                                \  For example, if you distribute copies of such a program, whether\n\
                                \gratis or for a fee, you must pass on to the recipients the same\n\
                                \freedoms that you received.  You must make sure that they, too, receive\n\
                                \or can get the source code.  And you must show them these terms so they\n\
                                \know their rights.\n\
                                \\n\
                                \  Developers that use the GNU GPL protect your rights with two steps:\n\
                                \(1) assert copyright on the software, and (2) offer you this License\n\
                                \giving you legal permission to copy, distribute and/or modify it.\n\
                                \\n\
                                \  For the developers' and authors' protection, the GPL clearly explains\n\
                                \that there is no warranty for this free software.  For both users' and\n\
                                \authors' sake, the GPL requires that modified versions be marked as\n\
                                \changed, so that their problems will not be attributed erroneously to\n\
                                \authors of previous versions.\n\
                                \\n\
                                \  Some devices are designed to deny users access to install or run\n\
                                \modified versions of the software inside them, although the manufacturer\n\
                                \can do so.  This is fundamentally incompatible with the aim of\n\
                                \protecting users' freedom to change the software.  The systematic\n\
                                \pattern of such abuse occurs in the area of products for individuals to\n\
                                \use, which is precisely where it is most unacceptable.  Therefore, we\n\
                                \have designed this version of the GPL to prohibit the practice for those\n\
                                \products.  If such problems arise substantially in other domains, we\n\
                                \stand ready to extend this provision to those domains in future versions\n\
                                \of the GPL, as needed to protect the freedom of users.\n\
                                \\n\
                                \  Finally, every program is threatened constantly by software patents.\n\
                                \States should not allow patents to restrict development and use of\n\
                                \software on general-purpose computers, but in those that do, we wish to\n\
                                \avoid the special danger that patents applied to a free program could\n\
                                \make it effectively proprietary.  To prevent this, the GPL assures that\n\
                                \patents cannot be used to render the program non-free.\n\
                                \\n\
                                \  The precise terms and conditions for copying, distribution and\n\
                                \modification follow.\n\
                                \\n\
                                \                       TERMS AND CONDITIONS\n\
                                \\n\
                                \  0. Definitions.\n\
                                \\n\
                                \  \"This License\" refers to version 3 of the GNU General Public License.\n\
                                \\n\
                                \  \"Copyright\" also means copyright-like laws that apply to other kinds of\n\
                                \works, such as semiconductor masks.\n\
                                \\n\
                                \  \"The Program\" refers to any copyrightable work licensed under this\n\
                                \License.  Each licensee is addressed as \"you\".  \"Licensees\" and\n\
                                \\"recipients\" may be individuals or organizations.\n\
                                \\n\
                                \  To \"modify\" a work means to copy from or adapt all or part of the work\n\
                                \in a fashion requiring copyright permission, other than the making of an\n\
                                \exact copy.  The resulting work is called a \"modified version\" of the\n\
                                \earlier work or a work \"based on\" the earlier work.\n\
                                \\n\
                                \  A \"covered work\" means either the unmodified Program or a work based\n\
                                \on the Program.\n\
                                \\n\
                                \  To \"propagate\" a work means to do anything with it that, without\n\
                                \permission, would make you directly or secondarily liable for\n\
                                \infringement under applicable copyright law, except executing it on a\n\
                                \computer or modifying a private copy.  Propagation includes copying,\n\
                                \distribution (with or without modification), making available to the\n\
                                \public, and in some countries other activities as well.\n\
                                \\n\
                                \  To \"convey\" a work means any kind of propagation that enables other\n\
                                \parties to make or receive copies.  Mere interaction with a user through\n\
                                \a computer network, with no transfer of a copy, is not conveying.\n\
                                \\n\
                                \  An interactive user interface displays \"Appropriate Legal Notices\"\n\
                                \to the extent that it includes a convenient and prominently visible\n\
                                \feature that (1) displays an appropriate copyright notice, and (2)\n\
                                \tells the user that there is no warranty for the work (except to the\n\
                                \extent that warranties are provided), that licensees may convey the\n\
                                \work under this License, and how to view a copy of this License.  If\n\
                                \the interface presents a list of user commands or options, such as a\n\
                                \menu, a prominent item in the list meets this criterion.\n\
                                \\n\
                                \  1. Source Code.\n\
                                \\n\
                                \  The \"source code\" for a work means the preferred form of the work\n\
                                \for making modifications to it.  \"Object code\" means any non-source\n\
                                \form of a work.\n\
                                \\n\
                                \  A \"Standard Interface\" means an interface that either is an official\n\
                                \standard defined by a recognized standards body, or, in the case of\n\
                                \interfaces specified for a particular programming language, one that\n\
                                \is widely used among developers working in that language.\n\
                                \\n\
                                \  The \"System Libraries\" of an executable work include anything, other\n\
                                \than the work as a whole, that (a) is included in the normal form of\n\
                                \packaging a Major Component, but which is not part of that Major\n\
                                \Component, and (b) serves only to enable use of the work with that\n\
                                \Major Component, or to implement a Standard Interface for which an\n\
                                \implementation is available to the public in source code form.  A\n\
                                \\"Major Component\", in this context, means a major essential component\n\
                                \(kernel, window system, and so on) of the specific operating system\n\
                                \(if any) on which the executable work runs, or a compiler used to\n\
                                \produce the work, or an object code interpreter used to run it.\n\
                                \\n\
                                \  The \"Corresponding Source\" for a work in object code form means all\n\
                                \the source code needed to generate, install, and (for an executable\n\
                                \work) run the object code and to modify the work, including scripts to\n\
                                \control those activities.  However, it does not include the work's\n\
                                \System Libraries, or general-purpose tools or generally available free\n\
                                \programs which are used unmodified in performing those activities but\n\
                                \which are not part of the work.  For example, Corresponding Source\n\
                                \includes interface definition files associated with source files for\n\
                                \the work, and the source code for shared libraries and dynamically\n\
                                \linked subprograms that the work is specifically designed to require,\n\
                                \such as by intimate data communication or control flow between those\n\
                                \subprograms and other parts of the work.\n\
                                \\n\
                                \  The Corresponding Source need not include anything that users\n\
                                \can regenerate automatically from other parts of the Corresponding\n\
                                \Source.\n\
                                \\n\
                                \  The Corresponding Source for a work in source code form is that\n\
                                \same work.\n\
                                \\n\
                                \  2. Basic Permissions.\n\
                                \\n\
                                \  All rights granted under this License are granted for the term of\n\
                                \copyright on the Program, and are irrevocable provided the stated\n\
                                \conditions are met.  This License explicitly affirms your unlimited\n\
                                \permission to run the unmodified Program.  The output from running a\n\
                                \covered work is covered by this License only if the output, given its\n\
                                \content, constitutes a covered work.  This License acknowledges your\n\
                                \rights of fair use or other equivalent, as provided by copyright law.\n\
                                \\n\
                                \  You may make, run and propagate covered works that you do not\n\
                                \convey, without conditions so long as your license otherwise remains\n\
                                \in force.  You may convey covered works to others for the sole purpose\n\
                                \of having them make modifications exclusively for you, or provide you\n\
                                \with facilities for running those works, provided that you comply with\n\
                                \the terms of this License in conveying all material for which you do\n\
                                \not control copyright.  Those thus making or running the covered works\n\
                                \for you must do so exclusively on your behalf, under your direction\n\
                                \and control, on terms that prohibit them from making any copies of\n\
                                \your copyrighted material outside their relationship with you.\n\
                                \\n\
                                \  Conveying under any other circumstances is permitted solely under\n\
                                \the conditions stated below.  Sublicensing is not allowed; section 10\n\
                                \makes it unnecessary.\n\
                                \\n\
                                \  3. Protecting Users' Legal Rights From Anti-Circumvention Law.\n\
                                \\n\
                                \  No covered work shall be deemed part of an effective technological\n\
                                \measure under any applicable law fulfilling obligations under article\n\
                                \11 of the WIPO copyright treaty adopted on 20 December 1996, or\n\
                                \similar laws prohibiting or restricting circumvention of such\n\
                                \measures.\n\
                                \\n\
                                \  When you convey a covered work, you waive any legal power to forbid\n\
                                \circumvention of technological measures to the extent such circumvention\n\
                                \is effected by exercising rights under this License with respect to\n\
                                \the covered work, and you disclaim any intention to limit operation or\n\
                                \modification of the work as a means of enforcing, against the work's\n\
                                \users, your or third parties' legal rights to forbid circumvention of\n\
                                \technological measures.\n\
                                \\n\
                                \  4. Conveying Verbatim Copies.\n\
                                \\n\
                                \  You may convey verbatim copies of the Program's source code as you\n\
                                \receive it, in any medium, provided that you conspicuously and\n\
                                \appropriately publish on each copy an appropriate copyright notice;\n\
                                \keep intact all notices stating that this License and any\n\
                                \non-permissive terms added in accord with section 7 apply to the code;\n\
                                \keep intact all notices of the absence of any warranty; and give all\n\
                                \recipients a copy of this License along with the Program.\n\
                                \\n\
                                \  You may charge any price or no price for each copy that you convey,\n\
                                \and you may offer support or warranty protection for a fee.\n\
                                \\n\
                                \  5. Conveying Modified Source Versions.\n\
                                \\n\
                                \  You may convey a work based on the Program, or the modifications to\n\
                                \produce it from the Program, in the form of source code under the\n\
                                \terms of section 4, provided that you also meet all of these conditions:\n\
                                \\n\
                                \    a) The work must carry prominent notices stating that you modified\n\
                                \    it, and giving a relevant date.\n\
                                \\n\
                                \    b) The work must carry prominent notices stating that it is\n\
                                \    released under this License and any conditions added under section\n\
                                \    7.  This requirement modifies the requirement in section 4 to\n\
                                \    \"keep intact all notices\".\n\
                                \\n\
                                \    c) You must license the entire work, as a whole, under this\n\
                                \    License to anyone who comes into possession of a copy.  This\n\
                                \    License will therefore apply, along with any applicable section 7\n\
                                \    additional terms, to the whole of the work, and all its parts,\n\
                                \    regardless of how they are packaged.  This License gives no\n\
                                \    permission to license the work in any other way, but it does not\n\
                                \    invalidate such permission if you have separately received it.\n\
                                \\n\
                                \    d) If the work has interactive user interfaces, each must display\n\
                                \    Appropriate Legal Notices; however, if the Program has interactive\n\
                                \    interfaces that do not display Appropriate Legal Notices, your\n\
                                \    work need not make them do so.\n\
                                \\n\
                                \  A compilation of a covered work with other separate and independent\n\
                                \works, which are not by their nature extensions of the covered work,\n\
                                \and which are not combined with it such as to form a larger program,\n\
                                \in or on a volume of a storage or distribution medium, is called an\n\
                                \\"aggregate\" if the compilation and its resulting copyright are not\n\
                                \used to limit the access or legal rights of the compilation's users\n\
                                \beyond what the individual works permit.  Inclusion of a covered work\n\
                                \in an aggregate does not cause this License to apply to the other\n\
                                \parts of the aggregate.\n\
                                \\n\
                                \  6. Conveying Non-Source Forms.\n\
                                \\n\
                                \  You may convey a covered work in object code form under the terms\n\
                                \of sections 4 and 5, provided that you also convey the\n\
                                \machine-readable Corresponding Source under the terms of this License,\n\
                                \in one of these ways:\n\
                                \\n\
                                \    a) Convey the object code in, or embodied in, a physical product\n\
                                \    (including a physical distribution medium), accompanied by the\n\
                                \    Corresponding Source fixed on a durable physical medium\n\
                                \    customarily used for software interchange.\n\
                                \\n\
                                \    b) Convey the object code in, or embodied in, a physical product\n\
                                \    (including a physical distribution medium), accompanied by a\n\
                                \    written offer, valid for at least three years and valid for as\n\
                                \    long as you offer spare parts or customer support for that product\n\
                                \    model, to give anyone who possesses the object code either (1) a\n\
                                \    copy of the Corresponding Source for all the software in the\n\
                                \    product that is covered by this License, on a durable physical\n\
                                \    medium customarily used for software interchange, for a price no\n\
                                \    more than your reasonable cost of physically performing this\n\
                                \    conveying of source, or (2) access to copy the\n\
                                \    Corresponding Source from a network server at no charge.\n\
                                \\n\
                                \    c) Convey individual copies of the object code with a copy of the\n\
                                \    written offer to provide the Corresponding Source.  This\n\
                                \    alternative is allowed only occasionally and noncommercially, and\n\
                                \    only if you received the object code with such an offer, in accord\n\
                                \    with subsection 6b.\n\
                                \\n\
                                \    d) Convey the object code by offering access from a designated\n\
                                \    place (gratis or for a charge), and offer equivalent access to the\n\
                                \    Corresponding Source in the same way through the same place at no\n\
                                \    further charge.  You need not require recipients to copy the\n\
                                \    Corresponding Source along with the object code.  If the place to\n\
                                \    copy the object code is a network server, the Corresponding Source\n\
                                \    may be on a different server (operated by you or a third party)\n\
                                \    that supports equivalent copying facilities, provided you maintain\n\
                                \    clear directions next to the object code saying where to find the\n\
                                \    Corresponding Source.  Regardless of what server hosts the\n\
                                \    Corresponding Source, you remain obligated to ensure that it is\n\
                                \    available for as long as needed to satisfy these requirements.\n\
                                \\n\
                                \    e) Convey the object code using peer-to-peer transmission, provided\n\
                                \    you inform other peers where the object code and Corresponding\n\
                                \    Source of the work are being offered to the general public at no\n\
                                \    charge under subsection 6d.\n\
                                \\n\
                                \  A separable portion of the object code, whose source code is excluded\n\
                                \from the Corresponding Source as a System Library, need not be\n\
                                \included in conveying the object code work.\n\
                                \\n\
                                \  A \"User Product\" is either (1) a \"consumer product\", which means any\n\
                                \tangible personal property which is normally used for personal, family,\n\
                                \or household purposes, or (2) anything designed or sold for incorporation\n\
                                \into a dwelling.  In determining whether a product is a consumer product,\n\
                                \doubtful cases shall be resolved in favor of coverage.  For a particular\n\
                                \product received by a particular user, \"normally used\" refers to a\n\
                                \typical or common use of that class of product, regardless of the status\n\
                                \of the particular user or of the way in which the particular user\n\
                                \actually uses, or expects or is expected to use, the product.  A product\n\
                                \is a consumer product regardless of whether the product has substantial\n\
                                \commercial, industrial or non-consumer uses, unless such uses represent\n\
                                \the only significant mode of use of the product.\n\
                                \\n\
                                \  \"Installation Information\" for a User Product means any methods,\n\
                                \procedures, authorization keys, or other information required to install\n\
                                \and execute modified versions of a covered work in that User Product from\n\
                                \a modified version of its Corresponding Source.  The information must\n\
                                \suffice to ensure that the continued functioning of the modified object\n\
                                \code is in no case prevented or interfered with solely because\n\
                                \modification has been made.\n\
                                \\n\
                                \  If you convey an object code work under this section in, or with, or\n\
                                \specifically for use in, a User Product, and the conveying occurs as\n\
                                \part of a transaction in which the right of possession and use of the\n\
                                \User Product is transferred to the recipient in perpetuity or for a\n\
                                \fixed term (regardless of how the transaction is characterized), the\n\
                                \Corresponding Source conveyed under this section must be accompanied\n\
                                \by the Installation Information.  But this requirement does not apply\n\
                                \if neither you nor any third party retains the ability to install\n\
                                \modified object code on the User Product (for example, the work has\n\
                                \been installed in ROM).\n\
                                \\n\
                                \  The requirement to provide Installation Information does not include a\n\
                                \requirement to continue to provide support service, warranty, or updates\n\
                                \for a work that has been modified or installed by the recipient, or for\n\
                                \the User Product in which it has been modified or installed.  Access to a\n\
                                \network may be denied when the modification itself materially and\n\
                                \adversely affects the operation of the network or violates the rules and\n\
                                \protocols for communication across the network.\n\
                                \\n\
                                \  Corresponding Source conveyed, and Installation Information provided,\n\
                                \in accord with this section must be in a format that is publicly\n\
                                \documented (and with an implementation available to the public in\n\
                                \source code form), and must require no special password or key for\n\
                                \unpacking, reading or copying.\n\
                                \\n\
                                \  7. Additional Terms.\n\
                                \\n\
                                \  \"Additional permissions\" are terms that supplement the terms of this\n\
                                \License by making exceptions from one or more of its conditions.\n\
                                \Additional permissions that are applicable to the entire Program shall\n\
                                \be treated as though they were included in this License, to the extent\n\
                                \that they are valid under applicable law.  If additional permissions\n\
                                \apply only to part of the Program, that part may be used separately\n\
                                \under those permissions, but the entire Program remains governed by\n\
                                \this License without regard to the additional permissions.\n\
                                \\n\
                                \  When you convey a copy of a covered work, you may at your option\n\
                                \remove any additional permissions from that copy, or from any part of\n\
                                \it.  (Additional permissions may be written to require their own\n\
                                \removal in certain cases when you modify the work.)  You may place\n\
                                \additional permissions on material, added by you to a covered work,\n\
                                \for which you have or can give appropriate copyright permission.\n\
                                \\n\
                                \  Notwithstanding any other provision of this License, for material you\n\
                                \add to a covered work, you may (if authorized by the copyright holders of\n\
                                \that material) supplement the terms of this License with terms:\n\
                                \\n\
                                \    a) Disclaiming warranty or limiting liability differently from the\n\
                                \    terms of sections 15 and 16 of this License; or\n\
                                \\n\
                                \    b) Requiring preservation of specified reasonable legal notices or\n\
                                \    author attributions in that material or in the Appropriate Legal\n\
                                \    Notices displayed by works containing it; or\n\
                                \\n\
                                \    c) Prohibiting misrepresentation of the origin of that material, or\n\
                                \    requiring that modified versions of such material be marked in\n\
                                \    reasonable ways as different from the original version; or\n\
                                \\n\
                                \    d) Limiting the use for publicity purposes of names of licensors or\n\
                                \    authors of the material; or\n\
                                \\n\
                                \    e) Declining to grant rights under trademark law for use of some\n\
                                \    trade names, trademarks, or service marks; or\n\
                                \\n\
                                \    f) Requiring indemnification of licensors and authors of that\n\
                                \    material by anyone who conveys the material (or modified versions of\n\
                                \    it) with contractual assumptions of liability to the recipient, for\n\
                                \    any liability that these contractual assumptions directly impose on\n\
                                \    those licensors and authors.\n\
                                \\n\
                                \  All other non-permissive additional terms are considered \"further\n\
                                \restrictions\" within the meaning of section 10.  If the Program as you\n\
                                \received it, or any part of it, contains a notice stating that it is\n\
                                \governed by this License along with a term that is a further\n\
                                \restriction, you may remove that term.  If a license document contains\n\
                                \a further restriction but permits relicensing or conveying under this\n\
                                \License, you may add to a covered work material governed by the terms\n\
                                \of that license document, provided that the further restriction does\n\
                                \not survive such relicensing or conveying.\n\
                                \\n\
                                \  If you add terms to a covered work in accord with this section, you\n\
                                \must place, in the relevant source files, a statement of the\n\
                                \additional terms that apply to those files, or a notice indicating\n\
                                \where to find the applicable terms.\n\
                                \\n\
                                \  Additional terms, permissive or non-permissive, may be stated in the\n\
                                \form of a separately written license, or stated as exceptions;\n\
                                \the above requirements apply either way.\n\
                                \\n\
                                \  8. Termination.\n\
                                \\n\
                                \  You may not propagate or modify a covered work except as expressly\n\
                                \provided under this License.  Any attempt otherwise to propagate or\n\
                                \modify it is void, and will automatically terminate your rights under\n\
                                \this License (including any patent licenses granted under the third\n\
                                \paragraph of section 11).\n\
                                \\n\
                                \  However, if you cease all violation of this License, then your\n\
                                \license from a particular copyright holder is reinstated (a)\n\
                                \provisionally, unless and until the copyright holder explicitly and\n\
                                \finally terminates your license, and (b) permanently, if the copyright\n\
                                \holder fails to notify you of the violation by some reasonable means\n\
                                \prior to 60 days after the cessation.\n\
                                \\n\
                                \  Moreover, your license from a particular copyright holder is\n\
                                \reinstated permanently if the copyright holder notifies you of the\n\
                                \violation by some reasonable means, this is the first time you have\n\
                                \received notice of violation of this License (for any work) from that\n\
                                \copyright holder, and you cure the violation prior to 30 days after\n\
                                \your receipt of the notice.\n\
                                \\n\
                                \  Termination of your rights under this section does not terminate the\n\
                                \licenses of parties who have received copies or rights from you under\n\
                                \this License.  If your rights have been terminated and not permanently\n\
                                \reinstated, you do not qualify to receive new licenses for the same\n\
                                \material under section 10.\n\
                                \\n\
                                \  9. Acceptance Not Required for Having Copies.\n\
                                \\n\
                                \  You are not required to accept this License in order to receive or\n\
                                \run a copy of the Program.  Ancillary propagation of a covered work\n\
                                \occurring solely as a consequence of using peer-to-peer transmission\n\
                                \to receive a copy likewise does not require acceptance.  However,\n\
                                \nothing other than this License grants you permission to propagate or\n\
                                \modify any covered work.  These actions infringe copyright if you do\n\
                                \not accept this License.  Therefore, by modifying or propagating a\n\
                                \covered work, you indicate your acceptance of this License to do so.\n\
                                \\n\
                                \  10. Automatic Licensing of Downstream Recipients.\n\
                                \\n\
                                \  Each time you convey a covered work, the recipient automatically\n\
                                \receives a license from the original licensors, to run, modify and\n\
                                \propagate that work, subject to this License.  You are not responsible\n\
                                \for enforcing compliance by third parties with this License.\n\
                                \\n\
                                \  An \"entity transaction\" is a transaction transferring control of an\n\
                                \organization, or substantially all assets of one, or subdividing an\n\
                                \organization, or merging organizations.  If propagation of a covered\n\
                                \work results from an entity transaction, each party to that\n\
                                \transaction who receives a copy of the work also receives whatever\n\
                                \licenses to the work the party's predecessor in interest had or could\n\
                                \give under the previous paragraph, plus a right to possession of the\n\
                                \Corresponding Source of the work from the predecessor in interest, if\n\
                                \the predecessor has it or can get it with reasonable efforts.\n\
                                \\n\
                                \  You may not impose any further restrictions on the exercise of the\n\
                                \rights granted or affirmed under this License.  For example, you may\n\
                                \not impose a license fee, royalty, or other charge for exercise of\n\
                                \rights granted under this License, and you may not initiate litigation\n\
                                \(including a cross-claim or counterclaim in a lawsuit) alleging that\n\
                                \any patent claim is infringed by making, using, selling, offering for\n\
                                \sale, or importing the Program or any portion of it.\n\
                                \\n\
                                \  11. Patents.\n\
                                \\n\
                                \  A \"contributor\" is a copyright holder who authorizes use under this\n\
                                \License of the Program or a work on which the Program is based.  The\n\
                                \work thus licensed is called the contributor's \"contributor version\".\n\
                                \\n\
                                \  A contributor's \"essential patent claims\" are all patent claims\n\
                                \owned or controlled by the contributor, whether already acquired or\n\
                                \hereafter acquired, that would be infringed by some manner, permitted\n\
                                \by this License, of making, using, or selling its contributor version,\n\
                                \but do not include claims that would be infringed only as a\n\
                                \consequence of further modification of the contributor version.  For\n\
                                \purposes of this definition, \"control\" includes the right to grant\n\
                                \patent sublicenses in a manner consistent with the requirements of\n\
                                \this License.\n\
                                \\n\
                                \  Each contributor grants you a non-exclusive, worldwide, royalty-free\n\
                                \patent license under the contributor's essential patent claims, to\n\
                                \make, use, sell, offer for sale, import and otherwise run, modify and\n\
                                \propagate the contents of its contributor version.\n\
                                \\n\
                                \  In the following three paragraphs, a \"patent license\" is any express\n\
                                \agreement or commitment, however denominated, not to enforce a patent\n\
                                \(such as an express permission to practice a patent or covenant not to\n\
                                \sue for patent infringement).  To \"grant\" such a patent license to a\n\
                                \party means to make such an agreement or commitment not to enforce a\n\
                                \patent against the party.\n\
                                \\n\
                                \  If you convey a covered work, knowingly relying on a patent license,\n\
                                \and the Corresponding Source of the work is not available for anyone\n\
                                \to copy, free of charge and under the terms of this License, through a\n\
                                \publicly available network server or other readily accessible means,\n\
                                \then you must either (1) cause the Corresponding Source to be so\n\
                                \available, or (2) arrange to deprive yourself of the benefit of the\n\
                                \patent license for this particular work, or (3) arrange, in a manner\n\
                                \consistent with the requirements of this License, to extend the patent\n\
                                \license to downstream recipients.  \"Knowingly relying\" means you have\n\
                                \actual knowledge that, but for the patent license, your conveying the\n\
                                \covered work in a country, or your recipient's use of the covered work\n\
                                \in a country, would infringe one or more identifiable patents in that\n\
                                \country that you have reason to believe are valid.\n\
                                \\n\
                                \  If, pursuant to or in connection with a single transaction or\n\
                                \arrangement, you convey, or propagate by procuring conveyance of, a\n\
                                \covered work, and grant a patent license to some of the parties\n\
                                \receiving the covered work authorizing them to use, propagate, modify\n\
                                \or convey a specific copy of the covered work, then the patent license\n\
                                \you grant is automatically extended to all recipients of the covered\n\
                                \work and works based on it.\n\
                                \\n\
                                \  A patent license is \"discriminatory\" if it does not include within\n\
                                \the scope of its coverage, prohibits the exercise of, or is\n\
                                \conditioned on the non-exercise of one or more of the rights that are\n\
                                \specifically granted under this License.  You may not convey a covered\n\
                                \work if you are a party to an arrangement with a third party that is\n\
                                \in the business of distributing software, under which you make payment\n\
                                \to the third party based on the extent of your activity of conveying\n\
                                \the work, and under which the third party grants, to any of the\n\
                                \parties who would receive the covered work from you, a discriminatory\n\
                                \patent license (a) in connection with copies of the covered work\n\
                                \conveyed by you (or copies made from those copies), or (b) primarily\n\
                                \for and in connection with specific products or compilations that\n\
                                \contain the covered work, unless you entered into that arrangement,\n\
                                \or that patent license was granted, prior to 28 March 2007.\n\
                                \\n\
                                \  Nothing in this License shall be construed as excluding or limiting\n\
                                \any implied license or other defenses to infringement that may\n\
                                \otherwise be available to you under applicable patent law.\n\
                                \\n\
                                \  12. No Surrender of Others' Freedom.\n\
                                \\n\
                                \  If conditions are imposed on you (whether by court order, agreement or\n\
                                \otherwise) that contradict the conditions of this License, they do not\n\
                                \excuse you from the conditions of this License.  If you cannot convey a\n\
                                \covered work so as to satisfy simultaneously your obligations under this\n\
                                \License and any other pertinent obligations, then as a consequence you may\n\
                                \not convey it at all.  For example, if you agree to terms that obligate you\n\
                                \to collect a royalty for further conveying from those to whom you convey\n\
                                \the Program, the only way you could satisfy both those terms and this\n\
                                \License would be to refrain entirely from conveying the Program.\n\
                                \\n\
                                \  13. Use with the GNU Affero General Public License.\n\
                                \\n\
                                \  Notwithstanding any other provision of this License, you have\n\
                                \permission to link or combine any covered work with a work licensed\n\
                                \under version 3 of the GNU Affero General Public License into a single\n\
                                \combined work, and to convey the resulting work.  The terms of this\n\
                                \License will continue to apply to the part which is the covered work,\n\
                                \but the special requirements of the GNU Affero General Public License,\n\
                                \section 13, concerning interaction through a network will apply to the\n\
                                \combination as such.\n\
                                \\n\
                                \  14. Revised Versions of this License.\n\
                                \\n\
                                \  The Free Software Foundation may publish revised and/or new versions of\n\
                                \the GNU General Public License from time to time.  Such new versions will\n\
                                \be similar in spirit to the present version, but may differ in detail to\n\
                                \address new problems or concerns.\n\
                                \\n\
                                \  Each version is given a distinguishing version number.  If the\n\
                                \Program specifies that a certain numbered version of the GNU General\n\
                                \Public License \"or any later version\" applies to it, you have the\n\
                                \option of following the terms and conditions either of that numbered\n\
                                \version or of any later version published by the Free Software\n\
                                \Foundation.  If the Program does not specify a version number of the\n\
                                \GNU General Public License, you may choose any version ever published\n\
                                \by the Free Software Foundation.\n\
                                \\n\
                                \  If the Program specifies that a proxy can decide which future\n\
                                \versions of the GNU General Public License can be used, that proxy's\n\
                                \public statement of acceptance of a version permanently authorizes you\n\
                                \to choose that version for the Program.\n\
                                \\n\
                                \  Later license versions may give you additional or different\n\
                                \permissions.  However, no additional obligations are imposed on any\n\
                                \author or copyright holder as a result of your choosing to follow a\n\
                                \later version.\n\
                                \\n\
                                \  15. Disclaimer of Warranty.\n\
                                \\n\
                                \  THERE IS NO WARRANTY FOR THE PROGRAM, TO THE EXTENT PERMITTED BY\n\
                                \APPLICABLE LAW.  EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT\n\
                                \HOLDERS AND/OR OTHER PARTIES PROVIDE THE PROGRAM \"AS IS\" WITHOUT WARRANTY\n\
                                \OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO,\n\
                                \THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR\n\
                                \PURPOSE.  THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE PROGRAM\n\
                                \IS WITH YOU.  SHOULD THE PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF\n\
                                \ALL NECESSARY SERVICING, REPAIR OR CORRECTION.\n\
                                \\n\
                                \  16. Limitation of Liability.\n\
                                \\n\
                                \  IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING\n\
                                \WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MODIFIES AND/OR CONVEYS\n\
                                \THE PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU FOR DAMAGES, INCLUDING ANY\n\
                                \GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE\n\
                                \USE OR INABILITY TO USE THE PROGRAM (INCLUDING BUT NOT LIMITED TO LOSS OF\n\
                                \DATA OR DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD\n\
                                \PARTIES OR A FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER PROGRAMS),\n\
                                \EVEN IF SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF\n\
                                \SUCH DAMAGES.\n\
                                \\n\
                                \  17. Interpretation of Sections 15 and 16.\n\
                                \\n\
                                \  If the disclaimer of warranty and limitation of liability provided\n\
                                \above cannot be given local legal effect according to their terms,\n\
                                \reviewing courts shall apply local law that most closely approximates\n\
                                \an absolute waiver of all civil liability in connection with the\n\
                                \Program, unless a warranty or assumption of liability accompanies a\n\
                                \copy of the Program in return for a fee.\n\
                                \\n\
                                \                     END OF TERMS AND CONDITIONS\n\
                                \\n\
                                \            How to Apply These Terms to Your New Programs\n\
                                \\n\
                                \  If you develop a new program, and you want it to be of the greatest\n\
                                \possible use to the public, the best way to achieve this is to make it\n\
                                \free software which everyone can redistribute and change under these terms.\n\
                                \\n\
                                \  To do so, attach the following notices to the program.  It is safest\n\
                                \to attach them to the start of each source file to most effectively\n\
                                \state the exclusion of warranty; and each file should have at least\n\
                                \the \"copyright\" line and a pointer to where the full notice is found.\n\
                                \\n\
                                \    <one line to give the program's name and a brief idea of what it does.>\n\
                                \    Copyright (C) <year>  <name of author>\n\
                                \\n\
                                \    This program is free software: you can redistribute it and/or modify\n\
                                \    it under the terms of the GNU General Public License as published by\n\
                                \    the Free Software Foundation, either version 3 of the License, or\n\
                                \    (at your option) any later version.\n\
                                \\n\
                                \    This program is distributed in the hope that it will be useful,\n\
                                \    but WITHOUT ANY WARRANTY; without even the implied warranty of\n\
                                \    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n\
                                \    GNU General Public License for more details.\n\
                                \\n\
                                \    You should have received a copy of the GNU General Public License\n\
                                \    along with this program.  If not, see <http://www.gnu.org/licenses/>.\n\
                                \\n\
                                \Also add information on how to contact you by electronic and paper mail.\n\
                                \\n\
                                \  If the program does terminal interaction, make it output a short\n\
                                \notice like this when it starts in an interactive mode:\n\
                                \\n\
                                \    <program>  Copyright (C) <year>  <name of author>\n\
                                \    This program comes with ABSOLUTELY NO WARRANTY; for details type `show w'.\n\
                                \    This is free software, and you are welcome to redistribute it\n\
                                \    under certain conditions; type `show c' for details.\n\
                                \\n\
                                \The hypothetical commands `show w' and `show c' should show the appropriate\n\
                                \parts of the General Public License.  Of course, your program's commands\n\
                                \might be different; for a GUI interface, you would use an \"about box\".\n\
                                \\n\
                                \  You should also get your employer (if you work as a programmer) or school,\n\
                                \if any, to sign a \"copyright disclaimer\" for the program, if necessary.\n\
                                \For more information on this, and how to apply and follow the GNU GPL, see\n\
                                \<http://www.gnu.org/licenses/>.\n\
                                \\n\
                                \  The GNU General Public License does not permit incorporating your program\n\
                                \into proprietary programs.  If your program is a subroutine library, you\n\
                                \may consider it more useful to permit linking proprietary applications with\n\
                                \the library.  If this is what you want to do, use the GNU Lesser General\n\
                                \Public License instead of this License.  But first, please read\n\
                                \<http://www.gnu.org/philosophy/why-not-lgpl.html>.\n"
licenseText AGPL              = "                    GNU AFFERO GENERAL PUBLIC LICENSE\n\
                                \                       Version 3, 19 November 2007\n\
                                \\n\
                                \ Copyright (C) 2007 Free Software Foundation, Inc. <http://fsf.org/>\n\
                                \ Everyone is permitted to copy and distribute verbatim copies\n\
                                \ of this license document, but changing it is not allowed.\n\
                                \\n\
                                \                            Preamble\n\
                                \\n\
                                \  The GNU Affero General Public License is a free, copyleft license for\n\
                                \software and other kinds of works, specifically designed to ensure\n\
                                \cooperation with the community in the case of network server software.\n\
                                \\n\
                                \  The licenses for most software and other practical works are designed\n\
                                \to take away your freedom to share and change the works.  By contrast,\n\
                                \our General Public Licenses are intended to guarantee your freedom to\n\
                                \share and change all versions of a program--to make sure it remains free\n\
                                \software for all its users.\n\
                                \\n\
                                \  When we speak of free software, we are referring to freedom, not\n\
                                \price.  Our General Public Licenses are designed to make sure that you\n\
                                \have the freedom to distribute copies of free software (and charge for\n\
                                \them if you wish), that you receive source code or can get it if you\n\
                                \want it, that you can change the software or use pieces of it in new\n\
                                \free programs, and that you know you can do these things.\n\
                                \\n\
                                \  Developers that use our General Public Licenses protect your rights\n\
                                \with two steps: (1) assert copyright on the software, and (2) offer\n\
                                \you this License which gives you legal permission to copy, distribute\n\
                                \and/or modify the software.\n\
                                \\n\
                                \  A secondary benefit of defending all users' freedom is that\n\
                                \improvements made in alternate versions of the program, if they\n\
                                \receive widespread use, become available for other developers to\n\
                                \incorporate.  Many developers of free software are heartened and\n\
                                \encouraged by the resulting cooperation.  However, in the case of\n\
                                \software used on network servers, this result may fail to come about.\n\
                                \The GNU General Public License permits making a modified version and\n\
                                \letting the public access it on a server without ever releasing its\n\
                                \source code to the public.\n\
                                \\n\
                                \  The GNU Affero General Public License is designed specifically to\n\
                                \ensure that, in such cases, the modified source code becomes available\n\
                                \to the community.  It requires the operator of a network server to\n\
                                \provide the source code of the modified version running there to the\n\
                                \users of that server.  Therefore, public use of a modified version, on\n\
                                \a publicly accessible server, gives the public access to the source\n\
                                \code of the modified version.\n\
                                \\n\
                                \  An older license, called the Affero General Public License and\n\
                                \published by Affero, was designed to accomplish similar goals.  This is\n\
                                \a different license, not a version of the Affero GPL, but Affero has\n\
                                \released a new version of the Affero GPL which permits relicensing under\n\
                                \this license.\n\
                                \\n\
                                \  The precise terms and conditions for copying, distribution and\n\
                                \modification follow.\n\
                                \\n\
                                \                       TERMS AND CONDITIONS\n\
                                \\n\
                                \  0. Definitions.\n\
                                \\n\
                                \  \"This License\" refers to version 3 of the GNU Affero General Public License.\n\
                                \\n\
                                \  \"Copyright\" also means copyright-like laws that apply to other kinds of\n\
                                \works, such as semiconductor masks.\n\
                                \\n\
                                \  \"The Program\" refers to any copyrightable work licensed under this\n\
                                \License.  Each licensee is addressed as \"you\".  \"Licensees\" and\n\
                                \\"recipients\" may be individuals or organizations.\n\
                                \\n\
                                \  To \"modify\" a work means to copy from or adapt all or part of the work\n\
                                \in a fashion requiring copyright permission, other than the making of an\n\
                                \exact copy.  The resulting work is called a \"modified version\" of the\n\
                                \earlier work or a work \"based on\" the earlier work.\n\
                                \\n\
                                \  A \"covered work\" means either the unmodified Program or a work based\n\
                                \on the Program.\n\
                                \\n\
                                \  To \"propagate\" a work means to do anything with it that, without\n\
                                \permission, would make you directly or secondarily liable for\n\
                                \infringement under applicable copyright law, except executing it on a\n\
                                \computer or modifying a private copy.  Propagation includes copying,\n\
                                \distribution (with or without modification), making available to the\n\
                                \public, and in some countries other activities as well.\n\
                                \\n\
                                \  To \"convey\" a work means any kind of propagation that enables other\n\
                                \parties to make or receive copies.  Mere interaction with a user through\n\
                                \a computer network, with no transfer of a copy, is not conveying.\n\
                                \\n\
                                \  An interactive user interface displays \"Appropriate Legal Notices\"\n\
                                \to the extent that it includes a convenient and prominently visible\n\
                                \feature that (1) displays an appropriate copyright notice, and (2)\n\
                                \tells the user that there is no warranty for the work (except to the\n\
                                \extent that warranties are provided), that licensees may convey the\n\
                                \work under this License, and how to view a copy of this License.  If\n\
                                \the interface presents a list of user commands or options, such as a\n\
                                \menu, a prominent item in the list meets this criterion.\n\
                                \\n\
                                \  1. Source Code.\n\
                                \\n\
                                \  The \"source code\" for a work means the preferred form of the work\n\
                                \for making modifications to it.  \"Object code\" means any non-source\n\
                                \form of a work.\n\
                                \\n\
                                \  A \"Standard Interface\" means an interface that either is an official\n\
                                \standard defined by a recognized standards body, or, in the case of\n\
                                \interfaces specified for a particular programming language, one that\n\
                                \is widely used among developers working in that language.\n\
                                \\n\
                                \  The \"System Libraries\" of an executable work include anything, other\n\
                                \than the work as a whole, that (a) is included in the normal form of\n\
                                \packaging a Major Component, but which is not part of that Major\n\
                                \Component, and (b) serves only to enable use of the work with that\n\
                                \Major Component, or to implement a Standard Interface for which an\n\
                                \implementation is available to the public in source code form.  A\n\
                                \\"Major Component\", in this context, means a major essential component\n\
                                \(kernel, window system, and so on) of the specific operating system\n\
                                \(if any) on which the executable work runs, or a compiler used to\n\
                                \produce the work, or an object code interpreter used to run it.\n\
                                \\n\
                                \  The \"Corresponding Source\" for a work in object code form means all\n\
                                \the source code needed to generate, install, and (for an executable\n\
                                \work) run the object code and to modify the work, including scripts to\n\
                                \control those activities.  However, it does not include the work's\n\
                                \System Libraries, or general-purpose tools or generally available free\n\
                                \programs which are used unmodified in performing those activities but\n\
                                \which are not part of the work.  For example, Corresponding Source\n\
                                \includes interface definition files associated with source files for\n\
                                \the work, and the source code for shared libraries and dynamically\n\
                                \linked subprograms that the work is specifically designed to require,\n\
                                \such as by intimate data communication or control flow between those\n\
                                \subprograms and other parts of the work.\n\
                                \\n\
                                \  The Corresponding Source need not include anything that users\n\
                                \can regenerate automatically from other parts of the Corresponding\n\
                                \Source.\n\
                                \\n\
                                \  The Corresponding Source for a work in source code form is that\n\
                                \same work.\n\
                                \\n\
                                \  2. Basic Permissions.\n\
                                \\n\
                                \  All rights granted under this License are granted for the term of\n\
                                \copyright on the Program, and are irrevocable provided the stated\n\
                                \conditions are met.  This License explicitly affirms your unlimited\n\
                                \permission to run the unmodified Program.  The output from running a\n\
                                \covered work is covered by this License only if the output, given its\n\
                                \content, constitutes a covered work.  This License acknowledges your\n\
                                \rights of fair use or other equivalent, as provided by copyright law.\n\
                                \\n\
                                \  You may make, run and propagate covered works that you do not\n\
                                \convey, without conditions so long as your license otherwise remains\n\
                                \in force.  You may convey covered works to others for the sole purpose\n\
                                \of having them make modifications exclusively for you, or provide you\n\
                                \with facilities for running those works, provided that you comply with\n\
                                \the terms of this License in conveying all material for which you do\n\
                                \not control copyright.  Those thus making or running the covered works\n\
                                \for you must do so exclusively on your behalf, under your direction\n\
                                \and control, on terms that prohibit them from making any copies of\n\
                                \your copyrighted material outside their relationship with you.\n\
                                \\n\
                                \  Conveying under any other circumstances is permitted solely under\n\
                                \the conditions stated below.  Sublicensing is not allowed; section 10\n\
                                \makes it unnecessary.\n\
                                \\n\
                                \  3. Protecting Users' Legal Rights From Anti-Circumvention Law.\n\
                                \\n\
                                \  No covered work shall be deemed part of an effective technological\n\
                                \measure under any applicable law fulfilling obligations under article\n\
                                \11 of the WIPO copyright treaty adopted on 20 December 1996, or\n\
                                \similar laws prohibiting or restricting circumvention of such\n\
                                \measures.\n\
                                \\n\
                                \  When you convey a covered work, you waive any legal power to forbid\n\
                                \circumvention of technological measures to the extent such circumvention\n\
                                \is effected by exercising rights under this License with respect to\n\
                                \the covered work, and you disclaim any intention to limit operation or\n\
                                \modification of the work as a means of enforcing, against the work's\n\
                                \users, your or third parties' legal rights to forbid circumvention of\n\
                                \technological measures.\n\
                                \\n\
                                \  4. Conveying Verbatim Copies.\n\
                                \\n\
                                \  You may convey verbatim copies of the Program's source code as you\n\
                                \receive it, in any medium, provided that you conspicuously and\n\
                                \appropriately publish on each copy an appropriate copyright notice;\n\
                                \keep intact all notices stating that this License and any\n\
                                \non-permissive terms added in accord with section 7 apply to the code;\n\
                                \keep intact all notices of the absence of any warranty; and give all\n\
                                \recipients a copy of this License along with the Program.\n\
                                \\n\
                                \  You may charge any price or no price for each copy that you convey,\n\
                                \and you may offer support or warranty protection for a fee.\n\
                                \\n\
                                \  5. Conveying Modified Source Versions.\n\
                                \\n\
                                \  You may convey a work based on the Program, or the modifications to\n\
                                \produce it from the Program, in the form of source code under the\n\
                                \terms of section 4, provided that you also meet all of these conditions:\n\
                                \\n\
                                \    a) The work must carry prominent notices stating that you modified\n\
                                \    it, and giving a relevant date.\n\
                                \\n\
                                \    b) The work must carry prominent notices stating that it is\n\
                                \    released under this License and any conditions added under section\n\
                                \    7.  This requirement modifies the requirement in section 4 to\n\
                                \    \"keep intact all notices\".\n\
                                \\n\
                                \    c) You must license the entire work, as a whole, under this\n\
                                \    License to anyone who comes into possession of a copy.  This\n\
                                \    License will therefore apply, along with any applicable section 7\n\
                                \    additional terms, to the whole of the work, and all its parts,\n\
                                \    regardless of how they are packaged.  This License gives no\n\
                                \    permission to license the work in any other way, but it does not\n\
                                \    invalidate such permission if you have separately received it.\n\
                                \\n\
                                \    d) If the work has interactive user interfaces, each must display\n\
                                \    Appropriate Legal Notices; however, if the Program has interactive\n\
                                \    interfaces that do not display Appropriate Legal Notices, your\n\
                                \    work need not make them do so.\n\
                                \\n\
                                \  A compilation of a covered work with other separate and independent\n\
                                \works, which are not by their nature extensions of the covered work,\n\
                                \and which are not combined with it such as to form a larger program,\n\
                                \in or on a volume of a storage or distribution medium, is called an\n\
                                \\"aggregate\" if the compilation and its resulting copyright are not\n\
                                \used to limit the access or legal rights of the compilation's users\n\
                                \beyond what the individual works permit.  Inclusion of a covered work\n\
                                \in an aggregate does not cause this License to apply to the other\n\
                                \parts of the aggregate.\n\
                                \\n\
                                \  6. Conveying Non-Source Forms.\n\
                                \\n\
                                \  You may convey a covered work in object code form under the terms\n\
                                \of sections 4 and 5, provided that you also convey the\n\
                                \machine-readable Corresponding Source under the terms of this License,\n\
                                \in one of these ways:\n\
                                \\n\
                                \    a) Convey the object code in, or embodied in, a physical product\n\
                                \    (including a physical distribution medium), accompanied by the\n\
                                \    Corresponding Source fixed on a durable physical medium\n\
                                \    customarily used for software interchange.\n\
                                \\n\
                                \    b) Convey the object code in, or embodied in, a physical product\n\
                                \    (including a physical distribution medium), accompanied by a\n\
                                \    written offer, valid for at least three years and valid for as\n\
                                \    long as you offer spare parts or customer support for that product\n\
                                \    model, to give anyone who possesses the object code either (1) a\n\
                                \    copy of the Corresponding Source for all the software in the\n\
                                \    product that is covered by this License, on a durable physical\n\
                                \    medium customarily used for software interchange, for a price no\n\
                                \    more than your reasonable cost of physically performing this\n\
                                \    conveying of source, or (2) access to copy the\n\
                                \    Corresponding Source from a network server at no charge.\n\
                                \\n\
                                \    c) Convey individual copies of the object code with a copy of the\n\
                                \    written offer to provide the Corresponding Source.  This\n\
                                \    alternative is allowed only occasionally and noncommercially, and\n\
                                \    only if you received the object code with such an offer, in accord\n\
                                \    with subsection 6b.\n\
                                \\n\
                                \    d) Convey the object code by offering access from a designated\n\
                                \    place (gratis or for a charge), and offer equivalent access to the\n\
                                \    Corresponding Source in the same way through the same place at no\n\
                                \    further charge.  You need not require recipients to copy the\n\
                                \    Corresponding Source along with the object code.  If the place to\n\
                                \    copy the object code is a network server, the Corresponding Source\n\
                                \    may be on a different server (operated by you or a third party)\n\
                                \    that supports equivalent copying facilities, provided you maintain\n\
                                \    clear directions next to the object code saying where to find the\n\
                                \    Corresponding Source.  Regardless of what server hosts the\n\
                                \    Corresponding Source, you remain obligated to ensure that it is\n\
                                \    available for as long as needed to satisfy these requirements.\n\
                                \\n\
                                \    e) Convey the object code using peer-to-peer transmission, provided\n\
                                \    you inform other peers where the object code and Corresponding\n\
                                \    Source of the work are being offered to the general public at no\n\
                                \    charge under subsection 6d.\n\
                                \\n\
                                \  A separable portion of the object code, whose source code is excluded\n\
                                \from the Corresponding Source as a System Library, need not be\n\
                                \included in conveying the object code work.\n\
                                \\n\
                                \  A \"User Product\" is either (1) a \"consumer product\", which means any\n\
                                \tangible personal property which is normally used for personal, family,\n\
                                \or household purposes, or (2) anything designed or sold for incorporation\n\
                                \into a dwelling.  In determining whether a product is a consumer product,\n\
                                \doubtful cases shall be resolved in favor of coverage.  For a particular\n\
                                \product received by a particular user, \"normally used\" refers to a\n\
                                \typical or common use of that class of product, regardless of the status\n\
                                \of the particular user or of the way in which the particular user\n\
                                \actually uses, or expects or is expected to use, the product.  A product\n\
                                \is a consumer product regardless of whether the product has substantial\n\
                                \commercial, industrial or non-consumer uses, unless such uses represent\n\
                                \the only significant mode of use of the product.\n\
                                \\n\
                                \  \"Installation Information\" for a User Product means any methods,\n\
                                \procedures, authorization keys, or other information required to install\n\
                                \and execute modified versions of a covered work in that User Product from\n\
                                \a modified version of its Corresponding Source.  The information must\n\
                                \suffice to ensure that the continued functioning of the modified object\n\
                                \code is in no case prevented or interfered with solely because\n\
                                \modification has been made.\n\
                                \\n\
                                \  If you convey an object code work under this section in, or with, or\n\
                                \specifically for use in, a User Product, and the conveying occurs as\n\
                                \part of a transaction in which the right of possession and use of the\n\
                                \User Product is transferred to the recipient in perpetuity or for a\n\
                                \fixed term (regardless of how the transaction is characterized), the\n\
                                \Corresponding Source conveyed under this section must be accompanied\n\
                                \by the Installation Information.  But this requirement does not apply\n\
                                \if neither you nor any third party retains the ability to install\n\
                                \modified object code on the User Product (for example, the work has\n\
                                \been installed in ROM).\n\
                                \\n\
                                \  The requirement to provide Installation Information does not include a\n\
                                \requirement to continue to provide support service, warranty, or updates\n\
                                \for a work that has been modified or installed by the recipient, or for\n\
                                \the User Product in which it has been modified or installed.  Access to a\n\
                                \network may be denied when the modification itself materially and\n\
                                \adversely affects the operation of the network or violates the rules and\n\
                                \protocols for communication across the network.\n\
                                \\n\
                                \  Corresponding Source conveyed, and Installation Information provided,\n\
                                \in accord with this section must be in a format that is publicly\n\
                                \documented (and with an implementation available to the public in\n\
                                \source code form), and must require no special password or key for\n\
                                \unpacking, reading or copying.\n\
                                \\n\
                                \  7. Additional Terms.\n\
                                \\n\
                                \  \"Additional permissions\" are terms that supplement the terms of this\n\
                                \License by making exceptions from one or more of its conditions.\n\
                                \Additional permissions that are applicable to the entire Program shall\n\
                                \be treated as though they were included in this License, to the extent\n\
                                \that they are valid under applicable law.  If additional permissions\n\
                                \apply only to part of the Program, that part may be used separately\n\
                                \under those permissions, but the entire Program remains governed by\n\
                                \this License without regard to the additional permissions.\n\
                                \\n\
                                \  When you convey a copy of a covered work, you may at your option\n\
                                \remove any additional permissions from that copy, or from any part of\n\
                                \it.  (Additional permissions may be written to require their own\n\
                                \removal in certain cases when you modify the work.)  You may place\n\
                                \additional permissions on material, added by you to a covered work,\n\
                                \for which you have or can give appropriate copyright permission.\n\
                                \\n\
                                \  Notwithstanding any other provision of this License, for material you\n\
                                \add to a covered work, you may (if authorized by the copyright holders of\n\
                                \that material) supplement the terms of this License with terms:\n\
                                \\n\
                                \    a) Disclaiming warranty or limiting liability differently from the\n\
                                \    terms of sections 15 and 16 of this License; or\n\
                                \\n\
                                \    b) Requiring preservation of specified reasonable legal notices or\n\
                                \    author attributions in that material or in the Appropriate Legal\n\
                                \    Notices displayed by works containing it; or\n\
                                \\n\
                                \    c) Prohibiting misrepresentation of the origin of that material, or\n\
                                \    requiring that modified versions of such material be marked in\n\
                                \    reasonable ways as different from the original version; or\n\
                                \\n\
                                \    d) Limiting the use for publicity purposes of names of licensors or\n\
                                \    authors of the material; or\n\
                                \\n\
                                \    e) Declining to grant rights under trademark law for use of some\n\
                                \    trade names, trademarks, or service marks; or\n\
                                \\n\
                                \    f) Requiring indemnification of licensors and authors of that\n\
                                \    material by anyone who conveys the material (or modified versions of\n\
                                \    it) with contractual assumptions of liability to the recipient, for\n\
                                \    any liability that these contractual assumptions directly impose on\n\
                                \    those licensors and authors.\n\
                                \\n\
                                \  All other non-permissive additional terms are considered \"further\n\
                                \restrictions\" within the meaning of section 10.  If the Program as you\n\
                                \received it, or any part of it, contains a notice stating that it is\n\
                                \governed by this License along with a term that is a further\n\
                                \restriction, you may remove that term.  If a license document contains\n\
                                \a further restriction but permits relicensing or conveying under this\n\
                                \License, you may add to a covered work material governed by the terms\n\
                                \of that license document, provided that the further restriction does\n\
                                \not survive such relicensing or conveying.\n\
                                \\n\
                                \  If you add terms to a covered work in accord with this section, you\n\
                                \must place, in the relevant source files, a statement of the\n\
                                \additional terms that apply to those files, or a notice indicating\n\
                                \where to find the applicable terms.\n\
                                \\n\
                                \  Additional terms, permissive or non-permissive, may be stated in the\n\
                                \form of a separately written license, or stated as exceptions;\n\
                                \the above requirements apply either way.\n\
                                \\n\
                                \  8. Termination.\n\
                                \\n\
                                \  You may not propagate or modify a covered work except as expressly\n\
                                \provided under this License.  Any attempt otherwise to propagate or\n\
                                \modify it is void, and will automatically terminate your rights under\n\
                                \this License (including any patent licenses granted under the third\n\
                                \paragraph of section 11).\n\
                                \\n\
                                \  However, if you cease all violation of this License, then your\n\
                                \license from a particular copyright holder is reinstated (a)\n\
                                \provisionally, unless and until the copyright holder explicitly and\n\
                                \finally terminates your license, and (b) permanently, if the copyright\n\
                                \holder fails to notify you of the violation by some reasonable means\n\
                                \prior to 60 days after the cessation.\n\
                                \\n\
                                \  Moreover, your license from a particular copyright holder is\n\
                                \reinstated permanently if the copyright holder notifies you of the\n\
                                \violation by some reasonable means, this is the first time you have\n\
                                \received notice of violation of this License (for any work) from that\n\
                                \copyright holder, and you cure the violation prior to 30 days after\n\
                                \your receipt of the notice.\n\
                                \\n\
                                \  Termination of your rights under this section does not terminate the\n\
                                \licenses of parties who have received copies or rights from you under\n\
                                \this License.  If your rights have been terminated and not permanently\n\
                                \reinstated, you do not qualify to receive new licenses for the same\n\
                                \material under section 10.\n\
                                \\n\
                                \  9. Acceptance Not Required for Having Copies.\n\
                                \\n\
                                \  You are not required to accept this License in order to receive or\n\
                                \run a copy of the Program.  Ancillary propagation of a covered work\n\
                                \occurring solely as a consequence of using peer-to-peer transmission\n\
                                \to receive a copy likewise does not require acceptance.  However,\n\
                                \nothing other than this License grants you permission to propagate or\n\
                                \modify any covered work.  These actions infringe copyright if you do\n\
                                \not accept this License.  Therefore, by modifying or propagating a\n\
                                \covered work, you indicate your acceptance of this License to do so.\n\
                                \\n\
                                \  10. Automatic Licensing of Downstream Recipients.\n\
                                \\n\
                                \  Each time you convey a covered work, the recipient automatically\n\
                                \receives a license from the original licensors, to run, modify and\n\
                                \propagate that work, subject to this License.  You are not responsible\n\
                                \for enforcing compliance by third parties with this License.\n\
                                \\n\
                                \  An \"entity transaction\" is a transaction transferring control of an\n\
                                \organization, or substantially all assets of one, or subdividing an\n\
                                \organization, or merging organizations.  If propagation of a covered\n\
                                \work results from an entity transaction, each party to that\n\
                                \transaction who receives a copy of the work also receives whatever\n\
                                \licenses to the work the party's predecessor in interest had or could\n\
                                \give under the previous paragraph, plus a right to possession of the\n\
                                \Corresponding Source of the work from the predecessor in interest, if\n\
                                \the predecessor has it or can get it with reasonable efforts.\n\
                                \\n\
                                \  You may not impose any further restrictions on the exercise of the\n\
                                \rights granted or affirmed under this License.  For example, you may\n\
                                \not impose a license fee, royalty, or other charge for exercise of\n\
                                \rights granted under this License, and you may not initiate litigation\n\
                                \(including a cross-claim or counterclaim in a lawsuit) alleging that\n\
                                \any patent claim is infringed by making, using, selling, offering for\n\
                                \sale, or importing the Program or any portion of it.\n\
                                \\n\
                                \  11. Patents.\n\
                                \\n\
                                \  A \"contributor\" is a copyright holder who authorizes use under this\n\
                                \License of the Program or a work on which the Program is based.  The\n\
                                \work thus licensed is called the contributor's \"contributor version\".\n\
                                \\n\
                                \  A contributor's \"essential patent claims\" are all patent claims\n\
                                \owned or controlled by the contributor, whether already acquired or\n\
                                \hereafter acquired, that would be infringed by some manner, permitted\n\
                                \by this License, of making, using, or selling its contributor version,\n\
                                \but do not include claims that would be infringed only as a\n\
                                \consequence of further modification of the contributor version.  For\n\
                                \purposes of this definition, \"control\" includes the right to grant\n\
                                \patent sublicenses in a manner consistent with the requirements of\n\
                                \this License.\n\
                                \\n\
                                \  Each contributor grants you a non-exclusive, worldwide, royalty-free\n\
                                \patent license under the contributor's essential patent claims, to\n\
                                \make, use, sell, offer for sale, import and otherwise run, modify and\n\
                                \propagate the contents of its contributor version.\n\
                                \\n\
                                \  In the following three paragraphs, a \"patent license\" is any express\n\
                                \agreement or commitment, however denominated, not to enforce a patent\n\
                                \(such as an express permission to practice a patent or covenant not to\n\
                                \sue for patent infringement).  To \"grant\" such a patent license to a\n\
                                \party means to make such an agreement or commitment not to enforce a\n\
                                \patent against the party.\n\
                                \\n\
                                \  If you convey a covered work, knowingly relying on a patent license,\n\
                                \and the Corresponding Source of the work is not available for anyone\n\
                                \to copy, free of charge and under the terms of this License, through a\n\
                                \publicly available network server or other readily accessible means,\n\
                                \then you must either (1) cause the Corresponding Source to be so\n\
                                \available, or (2) arrange to deprive yourself of the benefit of the\n\
                                \patent license for this particular work, or (3) arrange, in a manner\n\
                                \consistent with the requirements of this License, to extend the patent\n\
                                \license to downstream recipients.  \"Knowingly relying\" means you have\n\
                                \actual knowledge that, but for the patent license, your conveying the\n\
                                \covered work in a country, or your recipient's use of the covered work\n\
                                \in a country, would infringe one or more identifiable patents in that\n\
                                \country that you have reason to believe are valid.\n\
                                \\n\
                                \  If, pursuant to or in connection with a single transaction or\n\
                                \arrangement, you convey, or propagate by procuring conveyance of, a\n\
                                \covered work, and grant a patent license to some of the parties\n\
                                \receiving the covered work authorizing them to use, propagate, modify\n\
                                \or convey a specific copy of the covered work, then the patent license\n\
                                \you grant is automatically extended to all recipients of the covered\n\
                                \work and works based on it.\n\
                                \\n\
                                \  A patent license is \"discriminatory\" if it does not include within\n\
                                \the scope of its coverage, prohibits the exercise of, or is\n\
                                \conditioned on the non-exercise of one or more of the rights that are\n\
                                \specifically granted under this License.  You may not convey a covered\n\
                                \work if you are a party to an arrangement with a third party that is\n\
                                \in the business of distributing software, under which you make payment\n\
                                \to the third party based on the extent of your activity of conveying\n\
                                \the work, and under which the third party grants, to any of the\n\
                                \parties who would receive the covered work from you, a discriminatory\n\
                                \patent license (a) in connection with copies of the covered work\n\
                                \conveyed by you (or copies made from those copies), or (b) primarily\n\
                                \for and in connection with specific products or compilations that\n\
                                \contain the covered work, unless you entered into that arrangement,\n\
                                \or that patent license was granted, prior to 28 March 2007.\n\
                                \\n\
                                \  Nothing in this License shall be construed as excluding or limiting\n\
                                \any implied license or other defenses to infringement that may\n\
                                \otherwise be available to you under applicable patent law.\n\
                                \\n\
                                \  12. No Surrender of Others' Freedom.\n\
                                \\n\
                                \  If conditions are imposed on you (whether by court order, agreement or\n\
                                \otherwise) that contradict the conditions of this License, they do not\n\
                                \excuse you from the conditions of this License.  If you cannot convey a\n\
                                \covered work so as to satisfy simultaneously your obligations under this\n\
                                \License and any other pertinent obligations, then as a consequence you may\n\
                                \not convey it at all.  For example, if you agree to terms that obligate you\n\
                                \to collect a royalty for further conveying from those to whom you convey\n\
                                \the Program, the only way you could satisfy both those terms and this\n\
                                \License would be to refrain entirely from conveying the Program.\n\
                                \\n\
                                \  13. Remote Network Interaction; Use with the GNU General Public License.\n\
                                \\n\
                                \  Notwithstanding any other provision of this License, if you modify the\n\
                                \Program, your modified version must prominently offer all users\n\
                                \interacting with it remotely through a computer network (if your version\n\
                                \supports such interaction) an opportunity to receive the Corresponding\n\
                                \Source of your version by providing access to the Corresponding Source\n\
                                \from a network server at no charge, through some standard or customary\n\
                                \means of facilitating copying of software.  This Corresponding Source\n\
                                \shall include the Corresponding Source for any work covered by version 3\n\
                                \of the GNU General Public License that is incorporated pursuant to the\n\
                                \following paragraph.\n\
                                \\n\
                                \  Notwithstanding any other provision of this License, you have\n\
                                \permission to link or combine any covered work with a work licensed\n\
                                \under version 3 of the GNU General Public License into a single\n\
                                \combined work, and to convey the resulting work.  The terms of this\n\
                                \License will continue to apply to the part which is the covered work,\n\
                                \but the work with which it is combined will remain governed by version\n\
                                \3 of the GNU General Public License.\n\
                                \\n\
                                \  14. Revised Versions of this License.\n\
                                \\n\
                                \  The Free Software Foundation may publish revised and/or new versions of\n\
                                \the GNU Affero General Public License from time to time.  Such new versions\n\
                                \will be similar in spirit to the present version, but may differ in detail to\n\
                                \address new problems or concerns.\n\
                                \\n\
                                \  Each version is given a distinguishing version number.  If the\n\
                                \Program specifies that a certain numbered version of the GNU Affero General\n\
                                \Public License \"or any later version\" applies to it, you have the\n\
                                \option of following the terms and conditions either of that numbered\n\
                                \version or of any later version published by the Free Software\n\
                                \Foundation.  If the Program does not specify a version number of the\n\
                                \GNU Affero General Public License, you may choose any version ever published\n\
                                \by the Free Software Foundation.\n\
                                \\n\
                                \  If the Program specifies that a proxy can decide which future\n\
                                \versions of the GNU Affero General Public License can be used, that proxy's\n\
                                \public statement of acceptance of a version permanently authorizes you\n\
                                \to choose that version for the Program.\n\
                                \\n\
                                \  Later license versions may give you additional or different\n\
                                \permissions.  However, no additional obligations are imposed on any\n\
                                \author or copyright holder as a result of your choosing to follow a\n\
                                \later version.\n\
                                \\n\
                                \  15. Disclaimer of Warranty.\n\
                                \\n\
                                \  THERE IS NO WARRANTY FOR THE PROGRAM, TO THE EXTENT PERMITTED BY\n\
                                \APPLICABLE LAW.  EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT\n\
                                \HOLDERS AND/OR OTHER PARTIES PROVIDE THE PROGRAM \"AS IS\" WITHOUT WARRANTY\n\
                                \OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO,\n\
                                \THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR\n\
                                \PURPOSE.  THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE PROGRAM\n\
                                \IS WITH YOU.  SHOULD THE PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF\n\
                                \ALL NECESSARY SERVICING, REPAIR OR CORRECTION.\n\
                                \\n\
                                \  16. Limitation of Liability.\n\
                                \\n\
                                \  IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING\n\
                                \WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MODIFIES AND/OR CONVEYS\n\
                                \THE PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU FOR DAMAGES, INCLUDING ANY\n\
                                \GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE\n\
                                \USE OR INABILITY TO USE THE PROGRAM (INCLUDING BUT NOT LIMITED TO LOSS OF\n\
                                \DATA OR DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD\n\
                                \PARTIES OR A FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER PROGRAMS),\n\
                                \EVEN IF SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF\n\
                                \SUCH DAMAGES.\n\
                                \\n\
                                \  17. Interpretation of Sections 15 and 16.\n\
                                \\n\
                                \  If the disclaimer of warranty and limitation of liability provided\n\
                                \above cannot be given local legal effect according to their terms,\n\
                                \reviewing courts shall apply local law that most closely approximates\n\
                                \an absolute waiver of all civil liability in connection with the\n\
                                \Program, unless a warranty or assumption of liability accompanies a\n\
                                \copy of the Program in return for a fee.\n\
                                \\n\
                                \                     END OF TERMS AND CONDITIONS\n\
                                \\n\
                                \            How to Apply These Terms to Your New Programs\n\
                                \\n\
                                \  If you develop a new program, and you want it to be of the greatest\n\
                                \possible use to the public, the best way to achieve this is to make it\n\
                                \free software which everyone can redistribute and change under these terms.\n\
                                \\n\
                                \  To do so, attach the following notices to the program.  It is safest\n\
                                \to attach them to the start of each source file to most effectively\n\
                                \state the exclusion of warranty; and each file should have at least\n\
                                \the \"copyright\" line and a pointer to where the full notice is found.\n\
                                \\n\
                                \    <one line to give the program's name and a brief idea of what it does.>\n\
                                \    Copyright (C) <year>  <name of author>\n\
                                \\n\
                                \    This program is free software: you can redistribute it and/or modify\n\
                                \    it under the terms of the GNU Affero General Public License as published by\n\
                                \    the Free Software Foundation, either version 3 of the License, or\n\
                                \    (at your option) any later version.\n\
                                \\n\
                                \    This program is distributed in the hope that it will be useful,\n\
                                \    but WITHOUT ANY WARRANTY; without even the implied warranty of\n\
                                \    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n\
                                \    GNU Affero General Public License for more details.\n\
                                \\n\
                                \    You should have received a copy of the GNU Affero General Public License\n\
                                \    along with this program.  If not, see <http://www.gnu.org/licenses/>.\n\
                                \\n\
                                \Also add information on how to contact you by electronic and paper mail.\n\
                                \\n\
                                \  If your software can interact with users remotely through a computer\n\
                                \network, you should also make sure that it provides a way for users to\n\
                                \get its source.  For example, if your program is a web application, its\n\
                                \interface could display a \"Source\" link that leads users to an archive\n\
                                \of the code.  There are many ways you could offer source, and different\n\
                                \solutions will be better for different programs; see section 13 for the\n\
                                \specific requirements.\n\
                                \\n\
                                \  You should also get your employer (if you work as a programmer) or school,\n\
                                \if any, to sign a \"copyright disclaimer\" for the program, if necessary.\n\
                                \For more information on this, and how to apply and follow the GNU AGPL, see\n\
                                \<http://www.gnu.org/licenses/>.\n"
licenseText LGPL21            = "                  GNU LESSER GENERAL PUBLIC LICENSE\n\
                                \                       Version 2.1, February 1999\n\
                                \\n\
                                \ Copyright (C) 1991, 1999 Free Software Foundation, Inc.\n\
                                \ 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA\n\
                                \ Everyone is permitted to copy and distribute verbatim copies\n\
                                \ of this license document, but changing it is not allowed.\n\
                                \\n\
                                \[This is the first released version of the Lesser GPL.  It also counts\n\
                                \ as the successor of the GNU Library Public License, version 2, hence\n\
                                \ the version number 2.1.]\n\
                                \\n\
                                \                            Preamble\n\
                                \\n\
                                \  The licenses for most software are designed to take away your\n\
                                \freedom to share and change it.  By contrast, the GNU General Public\n\
                                \Licenses are intended to guarantee your freedom to share and change\n\
                                \free software--to make sure the software is free for all its users.\n\
                                \\n\
                                \  This license, the Lesser General Public License, applies to some\n\
                                \specially designated software packages--typically libraries--of the\n\
                                \Free Software Foundation and other authors who decide to use it.  You\n\
                                \can use it too, but we suggest you first think carefully about whether\n\
                                \this license or the ordinary General Public License is the better\n\
                                \strategy to use in any particular case, based on the explanations below.\n\
                                \\n\
                                \  When we speak of free software, we are referring to freedom of use,\n\
                                \not price.  Our General Public Licenses are designed to make sure that\n\
                                \you have the freedom to distribute copies of free software (and charge\n\
                                \for this service if you wish); that you receive source code or can get\n\
                                \it if you want it; that you can change the software and use pieces of\n\
                                \it in new free programs; and that you are informed that you can do\n\
                                \these things.\n\
                                \\n\
                                \  To protect your rights, we need to make restrictions that forbid\n\
                                \distributors to deny you these rights or to ask you to surrender these\n\
                                \rights.  These restrictions translate to certain responsibilities for\n\
                                \you if you distribute copies of the library or if you modify it.\n\
                                \\n\
                                \  For example, if you distribute copies of the library, whether gratis\n\
                                \or for a fee, you must give the recipients all the rights that we gave\n\
                                \you.  You must make sure that they, too, receive or can get the source\n\
                                \code.  If you link other code with the library, you must provide\n\
                                \complete object files to the recipients, so that they can relink them\n\
                                \with the library after making changes to the library and recompiling\n\
                                \it.  And you must show them these terms so they know their rights.\n\
                                \\n\
                                \  We protect your rights with a two-step method: (1) we copyright the\n\
                                \library, and (2) we offer you this license, which gives you legal\n\
                                \permission to copy, distribute and/or modify the library.\n\
                                \\n\
                                \  To protect each distributor, we want to make it very clear that\n\
                                \there is no warranty for the free library.  Also, if the library is\n\
                                \modified by someone else and passed on, the recipients should know\n\
                                \that what they have is not the original version, so that the original\n\
                                \author's reputation will not be affected by problems that might be\n\
                                \introduced by others.\n\
                                \\n\
                                \  Finally, software patents pose a constant threat to the existence of\n\
                                \any free program.  We wish to make sure that a company cannot\n\
                                \effectively restrict the users of a free program by obtaining a\n\
                                \restrictive license from a patent holder.  Therefore, we insist that\n\
                                \any patent license obtained for a version of the library must be\n\
                                \consistent with the full freedom of use specified in this license.\n\
                                \\n\
                                \  Most GNU software, including some libraries, is covered by the\n\
                                \ordinary GNU General Public License.  This license, the GNU Lesser\n\
                                \General Public License, applies to certain designated libraries, and\n\
                                \is quite different from the ordinary General Public License.  We use\n\
                                \this license for certain libraries in order to permit linking those\n\
                                \libraries into non-free programs.\n\
                                \\n\
                                \  When a program is linked with a library, whether statically or using\n\
                                \a shared library, the combination of the two is legally speaking a\n\
                                \combined work, a derivative of the original library.  The ordinary\n\
                                \General Public License therefore permits such linking only if the\n\
                                \entire combination fits its criteria of freedom.  The Lesser General\n\
                                \Public License permits more lax criteria for linking other code with\n\
                                \the library.\n\
                                \\n\
                                \  We call this license the \"Lesser\" General Public License because it\n\
                                \does Less to protect the user's freedom than the ordinary General\n\
                                \Public License.  It also provides other free software developers Less\n\
                                \of an advantage over competing non-free programs.  These disadvantages\n\
                                \are the reason we use the ordinary General Public License for many\n\
                                \libraries.  However, the Lesser license provides advantages in certain\n\
                                \special circumstances.\n\
                                \\n\
                                \  For example, on rare occasions, there may be a special need to\n\
                                \encourage the widest possible use of a certain library, so that it becomes\n\
                                \a de-facto standard.  To achieve this, non-free programs must be\n\
                                \allowed to use the library.  A more frequent case is that a free\n\
                                \library does the same job as widely used non-free libraries.  In this\n\
                                \case, there is little to gain by limiting the free library to free\n\
                                \software only, so we use the Lesser General Public License.\n\
                                \\n\
                                \  In other cases, permission to use a particular library in non-free\n\
                                \programs enables a greater number of people to use a large body of\n\
                                \free software.  For example, permission to use the GNU C Library in\n\
                                \non-free programs enables many more people to use the whole GNU\n\
                                \operating system, as well as its variant, the GNU/Linux operating\n\
                                \system.\n\
                                \\n\
                                \  Although the Lesser General Public License is Less protective of the\n\
                                \users' freedom, it does ensure that the user of a program that is\n\
                                \linked with the Library has the freedom and the wherewithal to run\n\
                                \that program using a modified version of the Library.\n\
                                \\n\
                                \  The precise terms and conditions for copying, distribution and\n\
                                \modification follow.  Pay close attention to the difference between a\n\
                                \\"work based on the library\" and a \"work that uses the library\".  The\n\
                                \former contains code derived from the library, whereas the latter must\n\
                                \be combined with the library in order to run.\n\
                                \\n\
                                \                  GNU LESSER GENERAL PUBLIC LICENSE\n\
                                \   TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION\n\
                                \\n\
                                \  0. This License Agreement applies to any software library or other\n\
                                \program which contains a notice placed by the copyright holder or\n\
                                \other authorized party saying it may be distributed under the terms of\n\
                                \this Lesser General Public License (also called \"this License\").\n\
                                \Each licensee is addressed as \"you\".\n\
                                \\n\
                                \  A \"library\" means a collection of software functions and/or data\n\
                                \prepared so as to be conveniently linked with application programs\n\
                                \(which use some of those functions and data) to form executables.\n\
                                \\n\
                                \  The \"Library\", below, refers to any such software library or work\n\
                                \which has been distributed under these terms.  A \"work based on the\n\
                                \Library\" means either the Library or any derivative work under\n\
                                \copyright law: that is to say, a work containing the Library or a\n\
                                \portion of it, either verbatim or with modifications and/or translated\n\
                                \straightforwardly into another language.  (Hereinafter, translation is\n\
                                \included without limitation in the term \"modification\".)\n\
                                \\n\
                                \  \"Source code\" for a work means the preferred form of the work for\n\
                                \making modifications to it.  For a library, complete source code means\n\
                                \all the source code for all modules it contains, plus any associated\n\
                                \interface definition files, plus the scripts used to control compilation\n\
                                \and installation of the library.\n\
                                \\n\
                                \  Activities other than copying, distribution and modification are not\n\
                                \covered by this License; they are outside its scope.  The act of\n\
                                \running a program using the Library is not restricted, and output from\n\
                                \such a program is covered only if its contents constitute a work based\n\
                                \on the Library (independent of the use of the Library in a tool for\n\
                                \writing it).  Whether that is true depends on what the Library does\n\
                                \and what the program that uses the Library does.\n\
                                \\n\
                                \  1. You may copy and distribute verbatim copies of the Library's\n\
                                \complete source code as you receive it, in any medium, provided that\n\
                                \you conspicuously and appropriately publish on each copy an\n\
                                \appropriate copyright notice and disclaimer of warranty; keep intact\n\
                                \all the notices that refer to this License and to the absence of any\n\
                                \warranty; and distribute a copy of this License along with the\n\
                                \Library.\n\
                                \\n\
                                \  You may charge a fee for the physical act of transferring a copy,\n\
                                \and you may at your option offer warranty protection in exchange for a\n\
                                \fee.\n\
                                \\n\
                                \  2. You may modify your copy or copies of the Library or any portion\n\
                                \of it, thus forming a work based on the Library, and copy and\n\
                                \distribute such modifications or work under the terms of Section 1\n\
                                \above, provided that you also meet all of these conditions:\n\
                                \\n\
                                \    a) The modified work must itself be a software library.\n\
                                \\n\
                                \    b) You must cause the files modified to carry prominent notices\n\
                                \    stating that you changed the files and the date of any change.\n\
                                \\n\
                                \    c) You must cause the whole of the work to be licensed at no\n\
                                \    charge to all third parties under the terms of this License.\n\
                                \\n\
                                \    d) If a facility in the modified Library refers to a function or a\n\
                                \    table of data to be supplied by an application program that uses\n\
                                \    the facility, other than as an argument passed when the facility\n\
                                \    is invoked, then you must make a good faith effort to ensure that,\n\
                                \    in the event an application does not supply such function or\n\
                                \    table, the facility still operates, and performs whatever part of\n\
                                \    its purpose remains meaningful.\n\
                                \\n\
                                \    (For example, a function in a library to compute square roots has\n\
                                \    a purpose that is entirely well-defined independent of the\n\
                                \    application.  Therefore, Subsection 2d requires that any\n\
                                \    application-supplied function or table used by this function must\n\
                                \    be optional: if the application does not supply it, the square\n\
                                \    root function must still compute square roots.)\n\
                                \\n\
                                \These requirements apply to the modified work as a whole.  If\n\
                                \identifiable sections of that work are not derived from the Library,\n\
                                \and can be reasonably considered independent and separate works in\n\
                                \themselves, then this License, and its terms, do not apply to those\n\
                                \sections when you distribute them as separate works.  But when you\n\
                                \distribute the same sections as part of a whole which is a work based\n\
                                \on the Library, the distribution of the whole must be on the terms of\n\
                                \this License, whose permissions for other licensees extend to the\n\
                                \entire whole, and thus to each and every part regardless of who wrote\n\
                                \it.\n\
                                \\n\
                                \Thus, it is not the intent of this section to claim rights or contest\n\
                                \your rights to work written entirely by you; rather, the intent is to\n\
                                \exercise the right to control the distribution of derivative or\n\
                                \collective works based on the Library.\n\
                                \\n\
                                \In addition, mere aggregation of another work not based on the Library\n\
                                \with the Library (or with a work based on the Library) on a volume of\n\
                                \a storage or distribution medium does not bring the other work under\n\
                                \the scope of this License.\n\
                                \\n\
                                \  3. You may opt to apply the terms of the ordinary GNU General Public\n\
                                \License instead of this License to a given copy of the Library.  To do\n\
                                \this, you must alter all the notices that refer to this License, so\n\
                                \that they refer to the ordinary GNU General Public License, version 2,\n\
                                \instead of to this License.  (If a newer version than version 2 of the\n\
                                \ordinary GNU General Public License has appeared, then you can specify\n\
                                \that version instead if you wish.)  Do not make any other change in\n\
                                \these notices.\n\
                                \\n\
                                \  Once this change is made in a given copy, it is irreversible for\n\
                                \that copy, so the ordinary GNU General Public License applies to all\n\
                                \subsequent copies and derivative works made from that copy.\n\
                                \\n\
                                \  This option is useful when you wish to copy part of the code of\n\
                                \the Library into a program that is not a library.\n\
                                \\n\
                                \  4. You may copy and distribute the Library (or a portion or\n\
                                \derivative of it, under Section 2) in object code or executable form\n\
                                \under the terms of Sections 1 and 2 above provided that you accompany\n\
                                \it with the complete corresponding machine-readable source code, which\n\
                                \must be distributed under the terms of Sections 1 and 2 above on a\n\
                                \medium customarily used for software interchange.\n\
                                \\n\
                                \  If distribution of object code is made by offering access to copy\n\
                                \from a designated place, then offering equivalent access to copy the\n\
                                \source code from the same place satisfies the requirement to\n\
                                \distribute the source code, even though third parties are not\n\
                                \compelled to copy the source along with the object code.\n\
                                \\n\
                                \  5. A program that contains no derivative of any portion of the\n\
                                \Library, but is designed to work with the Library by being compiled or\n\
                                \linked with it, is called a \"work that uses the Library\".  Such a\n\
                                \work, in isolation, is not a derivative work of the Library, and\n\
                                \therefore falls outside the scope of this License.\n\
                                \\n\
                                \  However, linking a \"work that uses the Library\" with the Library\n\
                                \creates an executable that is a derivative of the Library (because it\n\
                                \contains portions of the Library), rather than a \"work that uses the\n\
                                \library\".  The executable is therefore covered by this License.\n\
                                \Section 6 states terms for distribution of such executables.\n\
                                \\n\
                                \  When a \"work that uses the Library\" uses material from a header file\n\
                                \that is part of the Library, the object code for the work may be a\n\
                                \derivative work of the Library even though the source code is not.\n\
                                \Whether this is true is especially significant if the work can be\n\
                                \linked without the Library, or if the work is itself a library.  The\n\
                                \threshold for this to be true is not precisely defined by law.\n\
                                \\n\
                                \  If such an object file uses only numerical parameters, data\n\
                                \structure layouts and accessors, and small macros and small inline\n\
                                \functions (ten lines or less in length), then the use of the object\n\
                                \file is unrestricted, regardless of whether it is legally a derivative\n\
                                \work.  (Executables containing this object code plus portions of the\n\
                                \Library will still fall under Section 6.)\n\
                                \\n\
                                \  Otherwise, if the work is a derivative of the Library, you may\n\
                                \distribute the object code for the work under the terms of Section 6.\n\
                                \Any executables containing that work also fall under Section 6,\n\
                                \whether or not they are linked directly with the Library itself.\n\
                                \\n\
                                \  6. As an exception to the Sections above, you may also combine or\n\
                                \link a \"work that uses the Library\" with the Library to produce a\n\
                                \work containing portions of the Library, and distribute that work\n\
                                \under terms of your choice, provided that the terms permit\n\
                                \modification of the work for the customer's own use and reverse\n\
                                \engineering for debugging such modifications.\n\
                                \\n\
                                \  You must give prominent notice with each copy of the work that the\n\
                                \Library is used in it and that the Library and its use are covered by\n\
                                \this License.  You must supply a copy of this License.  If the work\n\
                                \during execution displays copyright notices, you must include the\n\
                                \copyright notice for the Library among them, as well as a reference\n\
                                \directing the user to the copy of this License.  Also, you must do one\n\
                                \of these things:\n\
                                \\n\
                                \    a) Accompany the work with the complete corresponding\n\
                                \    machine-readable source code for the Library including whatever\n\
                                \    changes were used in the work (which must be distributed under\n\
                                \    Sections 1 and 2 above); and, if the work is an executable linked\n\
                                \    with the Library, with the complete machine-readable \"work that\n\
                                \    uses the Library\", as object code and/or source code, so that the\n\
                                \    user can modify the Library and then relink to produce a modified\n\
                                \    executable containing the modified Library.  (It is understood\n\
                                \    that the user who changes the contents of definitions files in the\n\
                                \    Library will not necessarily be able to recompile the application\n\
                                \    to use the modified definitions.)\n\
                                \\n\
                                \    b) Use a suitable shared library mechanism for linking with the\n\
                                \    Library.  A suitable mechanism is one that (1) uses at run time a\n\
                                \    copy of the library already present on the user's computer system,\n\
                                \    rather than copying library functions into the executable, and (2)\n\
                                \    will operate properly with a modified version of the library, if\n\
                                \    the user installs one, as long as the modified version is\n\
                                \    interface-compatible with the version that the work was made with.\n\
                                \\n\
                                \    c) Accompany the work with a written offer, valid for at\n\
                                \    least three years, to give the same user the materials\n\
                                \    specified in Subsection 6a, above, for a charge no more\n\
                                \    than the cost of performing this distribution.\n\
                                \\n\
                                \    d) If distribution of the work is made by offering access to copy\n\
                                \    from a designated place, offer equivalent access to copy the above\n\
                                \    specified materials from the same place.\n\
                                \\n\
                                \    e) Verify that the user has already received a copy of these\n\
                                \    materials or that you have already sent this user a copy.\n\
                                \\n\
                                \  For an executable, the required form of the \"work that uses the\n\
                                \Library\" must include any data and utility programs needed for\n\
                                \reproducing the executable from it.  However, as a special exception,\n\
                                \the materials to be distributed need not include anything that is\n\
                                \normally distributed (in either source or binary form) with the major\n\
                                \components (compiler, kernel, and so on) of the operating system on\n\
                                \which the executable runs, unless that component itself accompanies\n\
                                \the executable.\n\
                                \\n\
                                \  It may happen that this requirement contradicts the license\n\
                                \restrictions of other proprietary libraries that do not normally\n\
                                \accompany the operating system.  Such a contradiction means you cannot\n\
                                \use both them and the Library together in an executable that you\n\
                                \distribute.\n\
                                \\n\
                                \  7. You may place library facilities that are a work based on the\n\
                                \Library side-by-side in a single library together with other library\n\
                                \facilities not covered by this License, and distribute such a combined\n\
                                \library, provided that the separate distribution of the work based on\n\
                                \the Library and of the other library facilities is otherwise\n\
                                \permitted, and provided that you do these two things:\n\
                                \\n\
                                \    a) Accompany the combined library with a copy of the same work\n\
                                \    based on the Library, uncombined with any other library\n\
                                \    facilities.  This must be distributed under the terms of the\n\
                                \    Sections above.\n\
                                \\n\
                                \    b) Give prominent notice with the combined library of the fact\n\
                                \    that part of it is a work based on the Library, and explaining\n\
                                \    where to find the accompanying uncombined form of the same work.\n\
                                \\n\
                                \  8. You may not copy, modify, sublicense, link with, or distribute\n\
                                \the Library except as expressly provided under this License.  Any\n\
                                \attempt otherwise to copy, modify, sublicense, link with, or\n\
                                \distribute the Library is void, and will automatically terminate your\n\
                                \rights under this License.  However, parties who have received copies,\n\
                                \or rights, from you under this License will not have their licenses\n\
                                \terminated so long as such parties remain in full compliance.\n\
                                \\n\
                                \  9. You are not required to accept this License, since you have not\n\
                                \signed it.  However, nothing else grants you permission to modify or\n\
                                \distribute the Library or its derivative works.  These actions are\n\
                                \prohibited by law if you do not accept this License.  Therefore, by\n\
                                \modifying or distributing the Library (or any work based on the\n\
                                \Library), you indicate your acceptance of this License to do so, and\n\
                                \all its terms and conditions for copying, distributing or modifying\n\
                                \the Library or works based on it.\n\
                                \\n\
                                \  10. Each time you redistribute the Library (or any work based on the\n\
                                \Library), the recipient automatically receives a license from the\n\
                                \original licensor to copy, distribute, link with or modify the Library\n\
                                \subject to these terms and conditions.  You may not impose any further\n\
                                \restrictions on the recipients' exercise of the rights granted herein.\n\
                                \You are not responsible for enforcing compliance by third parties with\n\
                                \this License.\n\
                                \\n\
                                \  11. If, as a consequence of a court judgment or allegation of patent\n\
                                \infringement or for any other reason (not limited to patent issues),\n\
                                \conditions are imposed on you (whether by court order, agreement or\n\
                                \otherwise) that contradict the conditions of this License, they do not\n\
                                \excuse you from the conditions of this License.  If you cannot\n\
                                \distribute so as to satisfy simultaneously your obligations under this\n\
                                \License and any other pertinent obligations, then as a consequence you\n\
                                \may not distribute the Library at all.  For example, if a patent\n\
                                \license would not permit royalty-free redistribution of the Library by\n\
                                \all those who receive copies directly or indirectly through you, then\n\
                                \the only way you could satisfy both it and this License would be to\n\
                                \refrain entirely from distribution of the Library.\n\
                                \\n\
                                \If any portion of this section is held invalid or unenforceable under any\n\
                                \particular circumstance, the balance of the section is intended to apply,\n\
                                \and the section as a whole is intended to apply in other circumstances.\n\
                                \\n\
                                \It is not the purpose of this section to induce you to infringe any\n\
                                \patents or other property right claims or to contest validity of any\n\
                                \such claims; this section has the sole purpose of protecting the\n\
                                \integrity of the free software distribution system which is\n\
                                \implemented by public license practices.  Many people have made\n\
                                \generous contributions to the wide range of software distributed\n\
                                \through that system in reliance on consistent application of that\n\
                                \system; it is up to the author/donor to decide if he or she is willing\n\
                                \to distribute software through any other system and a licensee cannot\n\
                                \impose that choice.\n\
                                \\n\
                                \This section is intended to make thoroughly clear what is believed to\n\
                                \be a consequence of the rest of this License.\n\
                                \\n\
                                \  12. If the distribution and/or use of the Library is restricted in\n\
                                \certain countries either by patents or by copyrighted interfaces, the\n\
                                \original copyright holder who places the Library under this License may add\n\
                                \an explicit geographical distribution limitation excluding those countries,\n\
                                \so that distribution is permitted only in or among countries not thus\n\
                                \excluded.  In such case, this License incorporates the limitation as if\n\
                                \written in the body of this License.\n\
                                \\n\
                                \  13. The Free Software Foundation may publish revised and/or new\n\
                                \versions of the Lesser General Public License from time to time.\n\
                                \Such new versions will be similar in spirit to the present version,\n\
                                \but may differ in detail to address new problems or concerns.\n\
                                \\n\
                                \Each version is given a distinguishing version number.  If the Library\n\
                                \specifies a version number of this License which applies to it and\n\
                                \\"any later version\", you have the option of following the terms and\n\
                                \conditions either of that version or of any later version published by\n\
                                \the Free Software Foundation.  If the Library does not specify a\n\
                                \license version number, you may choose any version ever published by\n\
                                \the Free Software Foundation.\n\
                                \\n\
                                \  14. If you wish to incorporate parts of the Library into other free\n\
                                \programs whose distribution conditions are incompatible with these,\n\
                                \write to the author to ask for permission.  For software which is\n\
                                \copyrighted by the Free Software Foundation, write to the Free\n\
                                \Software Foundation; we sometimes make exceptions for this.  Our\n\
                                \decision will be guided by the two goals of preserving the free status\n\
                                \of all derivatives of our free software and of promoting the sharing\n\
                                \and reuse of software generally.\n\
                                \\n\
                                \                            NO WARRANTY\n\
                                \\n\
                                \  15. BECAUSE THE LIBRARY IS LICENSED FREE OF CHARGE, THERE IS NO\n\
                                \WARRANTY FOR THE LIBRARY, TO THE EXTENT PERMITTED BY APPLICABLE LAW.\n\
                                \EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT HOLDERS AND/OR\n\
                                \OTHER PARTIES PROVIDE THE LIBRARY \"AS IS\" WITHOUT WARRANTY OF ANY\n\
                                \KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE\n\
                                \IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR\n\
                                \PURPOSE.  THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE\n\
                                \LIBRARY IS WITH YOU.  SHOULD THE LIBRARY PROVE DEFECTIVE, YOU ASSUME\n\
                                \THE COST OF ALL NECESSARY SERVICING, REPAIR OR CORRECTION.\n\
                                \\n\
                                \  16. IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN\n\
                                \WRITING WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MAY MODIFY\n\
                                \AND/OR REDISTRIBUTE THE LIBRARY AS PERMITTED ABOVE, BE LIABLE TO YOU\n\
                                \FOR DAMAGES, INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL OR\n\
                                \CONSEQUENTIAL DAMAGES ARISING OUT OF THE USE OR INABILITY TO USE THE\n\
                                \LIBRARY (INCLUDING BUT NOT LIMITED TO LOSS OF DATA OR DATA BEING\n\
                                \RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD PARTIES OR A\n\
                                \FAILURE OF THE LIBRARY TO OPERATE WITH ANY OTHER SOFTWARE), EVEN IF\n\
                                \SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH\n\
                                \DAMAGES.\n\
                                \\n\
                                \                     END OF TERMS AND CONDITIONS\n\
                                \\n\
                                \           How to Apply These Terms to Your New Libraries\n\
                                \\n\
                                \  If you develop a new library, and you want it to be of the greatest\n\
                                \possible use to the public, we recommend making it free software that\n\
                                \everyone can redistribute and change.  You can do so by permitting\n\
                                \redistribution under these terms (or, alternatively, under the terms of the\n\
                                \ordinary General Public License).\n\
                                \\n\
                                \  To apply these terms, attach the following notices to the library.  It is\n\
                                \safest to attach them to the start of each source file to most effectively\n\
                                \convey the exclusion of warranty; and each file should have at least the\n\
                                \\"copyright\" line and a pointer to where the full notice is found.\n\
                                \\n\
                                \    <one line to give the library's name and a brief idea of what it does.>\n\
                                \    Copyright (C) <year>  <name of author>\n\
                                \\n\
                                \    This library is free software; you can redistribute it and/or\n\
                                \    modify it under the terms of the GNU Lesser General Public\n\
                                \    License as published by the Free Software Foundation; either\n\
                                \    version 2.1 of the License, or (at your option) any later version.\n\
                                \\n\
                                \    This library is distributed in the hope that it will be useful,\n\
                                \    but WITHOUT ANY WARRANTY; without even the implied warranty of\n\
                                \    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU\n\
                                \    Lesser General Public License for more details.\n\
                                \\n\
                                \    You should have received a copy of the GNU Lesser General Public\n\
                                \    License along with this library; if not, write to the Free Software\n\
                                \    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA\n\
                                \\n\
                                \Also add information on how to contact you by electronic and paper mail.\n\
                                \\n\
                                \You should also get your employer (if you work as a programmer) or your\n\
                                \school, if any, to sign a \"copyright disclaimer\" for the library, if\n\
                                \necessary.  Here is a sample; alter the names:\n\
                                \\n\
                                \  Yoyodyne, Inc., hereby disclaims all copyright interest in the\n\
                                \  library `Frob' (a library for tweaking knobs) written by James Random Hacker.\n\
                                \\n\
                                \  <signature of Ty Coon>, 1 April 1990\n\
                                \  Ty Coon, President of Vice\n\
                                \\n\
                                \That's all there is to it!\n"
licenseText LGPL3             = "                   GNU LESSER GENERAL PUBLIC LICENSE\n\
                                \                       Version 3, 29 June 2007\n\
                                \\n\
                                \ Copyright (C) 2007 Free Software Foundation, Inc. <http://fsf.org/>\n\
                                \ Everyone is permitted to copy and distribute verbatim copies\n\
                                \ of this license document, but changing it is not allowed.\n\
                                \\n\
                                \\n\
                                \  This version of the GNU Lesser General Public License incorporates\n\
                                \the terms and conditions of version 3 of the GNU General Public\n\
                                \License, supplemented by the additional permissions listed below.\n\
                                \\n\
                                \  0. Additional Definitions.\n\
                                \\n\
                                \  As used herein, \"this License\" refers to version 3 of the GNU Lesser\n\
                                \General Public License, and the \"GNU GPL\" refers to version 3 of the GNU\n\
                                \General Public License.\n\
                                \\n\
                                \  \"The Library\" refers to a covered work governed by this License,\n\
                                \other than an Application or a Combined Work as defined below.\n\
                                \\n\
                                \  An \"Application\" is any work that makes use of an interface provided\n\
                                \by the Library, but which is not otherwise based on the Library.\n\
                                \Defining a subclass of a class defined by the Library is deemed a mode\n\
                                \of using an interface provided by the Library.\n\
                                \\n\
                                \  A \"Combined Work\" is a work produced by combining or linking an\n\
                                \Application with the Library.  The particular version of the Library\n\
                                \with which the Combined Work was made is also called the \"Linked\n\
                                \Version\".\n\
                                \\n\
                                \  The \"Minimal Corresponding Source\" for a Combined Work means the\n\
                                \Corresponding Source for the Combined Work, excluding any source code\n\
                                \for portions of the Combined Work that, considered in isolation, are\n\
                                \based on the Application, and not on the Linked Version.\n\
                                \\n\
                                \  The \"Corresponding Application Code\" for a Combined Work means the\n\
                                \object code and/or source code for the Application, including any data\n\
                                \and utility programs needed for reproducing the Combined Work from the\n\
                                \Application, but excluding the System Libraries of the Combined Work.\n\
                                \\n\
                                \  1. Exception to Section 3 of the GNU GPL.\n\
                                \\n\
                                \  You may convey a covered work under sections 3 and 4 of this License\n\
                                \without being bound by section 3 of the GNU GPL.\n\
                                \\n\
                                \  2. Conveying Modified Versions.\n\
                                \\n\
                                \  If you modify a copy of the Library, and, in your modifications, a\n\
                                \facility refers to a function or data to be supplied by an Application\n\
                                \that uses the facility (other than as an argument passed when the\n\
                                \facility is invoked), then you may convey a copy of the modified\n\
                                \version:\n\
                                \\n\
                                \   a) under this License, provided that you make a good faith effort to\n\
                                \   ensure that, in the event an Application does not supply the\n\
                                \   function or data, the facility still operates, and performs\n\
                                \   whatever part of its purpose remains meaningful, or\n\
                                \\n\
                                \   b) under the GNU GPL, with none of the additional permissions of\n\
                                \   this License applicable to that copy.\n\
                                \\n\
                                \  3. Object Code Incorporating Material from Library Header Files.\n\
                                \\n\
                                \  The object code form of an Application may incorporate material from\n\
                                \a header file that is part of the Library.  You may convey such object\n\
                                \code under terms of your choice, provided that, if the incorporated\n\
                                \material is not limited to numerical parameters, data structure\n\
                                \layouts and accessors, or small macros, inline functions and templates\n\
                                \(ten or fewer lines in length), you do both of the following:\n\
                                \\n\
                                \   a) Give prominent notice with each copy of the object code that the\n\
                                \   Library is used in it and that the Library and its use are\n\
                                \   covered by this License.\n\
                                \\n\
                                \   b) Accompany the object code with a copy of the GNU GPL and this license\n\
                                \   document.\n\
                                \\n\
                                \  4. Combined Works.\n\
                                \\n\
                                \  You may convey a Combined Work under terms of your choice that,\n\
                                \taken together, effectively do not restrict modification of the\n\
                                \portions of the Library contained in the Combined Work and reverse\n\
                                \engineering for debugging such modifications, if you also do each of\n\
                                \the following:\n\
                                \\n\
                                \   a) Give prominent notice with each copy of the Combined Work that\n\
                                \   the Library is used in it and that the Library and its use are\n\
                                \   covered by this License.\n\
                                \\n\
                                \   b) Accompany the Combined Work with a copy of the GNU GPL and this license\n\
                                \   document.\n\
                                \\n\
                                \   c) For a Combined Work that displays copyright notices during\n\
                                \   execution, include the copyright notice for the Library among\n\
                                \   these notices, as well as a reference directing the user to the\n\
                                \   copies of the GNU GPL and this license document.\n\
                                \\n\
                                \   d) Do one of the following:\n\
                                \\n\
                                \       0) Convey the Minimal Corresponding Source under the terms of this\n\
                                \       License, and the Corresponding Application Code in a form\n\
                                \       suitable for, and under terms that permit, the user to\n\
                                \       recombine or relink the Application with a modified version of\n\
                                \       the Linked Version to produce a modified Combined Work, in the\n\
                                \       manner specified by section 6 of the GNU GPL for conveying\n\
                                \       Corresponding Source.\n\
                                \\n\
                                \       1) Use a suitable shared library mechanism for linking with the\n\
                                \       Library.  A suitable mechanism is one that (a) uses at run time\n\
                                \       a copy of the Library already present on the user's computer\n\
                                \       system, and (b) will operate properly with a modified version\n\
                                \       of the Library that is interface-compatible with the Linked\n\
                                \       Version.\n\
                                \\n\
                                \   e) Provide Installation Information, but only if you would otherwise\n\
                                \   be required to provide such information under section 6 of the\n\
                                \   GNU GPL, and only to the extent that such information is\n\
                                \   necessary to install and execute a modified version of the\n\
                                \   Combined Work produced by recombining or relinking the\n\
                                \   Application with a modified version of the Linked Version. (If\n\
                                \   you use option 4d0, the Installation Information must accompany\n\
                                \   the Minimal Corresponding Source and Corresponding Application\n\
                                \   Code. If you use option 4d1, you must provide the Installation\n\
                                \   Information in the manner specified by section 6 of the GNU GPL\n\
                                \   for conveying Corresponding Source.)\n\
                                \\n\
                                \  5. Combined Libraries.\n\
                                \\n\
                                \  You may place library facilities that are a work based on the\n\
                                \Library side by side in a single library together with other library\n\
                                \facilities that are not Applications and are not covered by this\n\
                                \License, and convey such a combined library under terms of your\n\
                                \choice, if you do both of the following:\n\
                                \\n\
                                \   a) Accompany the combined library with a copy of the same work based\n\
                                \   on the Library, uncombined with any other library facilities,\n\
                                \   conveyed under the terms of this License.\n\
                                \\n\
                                \   b) Give prominent notice with the combined library that part of it\n\
                                \   is a work based on the Library, and explaining where to find the\n\
                                \   accompanying uncombined form of the same work.\n\
                                \\n\
                                \  6. Revised Versions of the GNU Lesser General Public License.\n\
                                \\n\
                                \  The Free Software Foundation may publish revised and/or new versions\n\
                                \of the GNU Lesser General Public License from time to time. Such new\n\
                                \versions will be similar in spirit to the present version, but may\n\
                                \differ in detail to address new problems or concerns.\n\
                                \\n\
                                \  Each version is given a distinguishing version number. If the\n\
                                \Library as you received it specifies that a certain numbered version\n\
                                \of the GNU Lesser General Public License \"or any later version\"\n\
                                \applies to it, you have the option of following the terms and\n\
                                \conditions either of that published version or of any later version\n\
                                \published by the Free Software Foundation. If the Library as you\n\
                                \received it does not specify a version number of the GNU Lesser\n\
                                \General Public License, you may choose any version of the GNU Lesser\n\
                                \General Public License ever published by the Free Software Foundation.\n\
                                \\n\
                                \  If the Library as you received it specifies that a proxy can decide\n\
                                \whether future versions of the GNU Lesser General Public License shall\n\
                                \apply, that proxy's public statement of acceptance of any version is\n\
                                \permanent authorization for you to choose that version for the\n\
                                \Library.\n"
licenseText BSD2              = "BSD 2-Clause License\n\
                                \\n\
                                \Redistribution and use in source and binary forms, with or without\n\
                                \modification, are permitted provided that the following conditions are met:\n\
                                \\n\
                                \* Redistributions of source code must retain the above copyright notice, this\n\
                                \  list of conditions and the following disclaimer.\n\
                                \\n\
                                \* Redistributions in binary form must reproduce the above copyright notice,\n\
                                \  this list of conditions and the following disclaimer in the documentation\n\
                                \  and/or other materials provided with the distribution.\n\
                                \\n\
                                \THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS \"AS IS\"\n\
                                \AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE\n\
                                \IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE\n\
                                \DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE\n\
                                \FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL\n\
                                \DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR\n\
                                \SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER\n\
                                \CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,\n\
                                \OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE\n\
                                \OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.\n"
licenseText BSD3              = "BSD 3-Clause License\n\
                                \\n\
                                \All rights reserved.\n\
                                \\n\
                                \Redistribution and use in source and binary forms, with or without\n\
                                \modification, are permitted provided that the following conditions are met:\n\
                                \\n\
                                \* Redistributions of source code must retain the above copyright notice, this\n\
                                \  list of conditions and the following disclaimer.\n\
                                \\n\
                                \* Redistributions in binary form must reproduce the above copyright notice,\n\
                                \  this list of conditions and the following disclaimer in the documentation\n\
                                \    and/or other materials provided with the distribution.\n\
                                \\n\
                                \* Neither the name of the copyright holder nor the names of its\n\
                                \  contributors may be used to endorse or promote products derived from\n\
                                \  this software without specific prior written permission.\n\
                                \\n\
                                \THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS \"AS IS\"\n\
                                \AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE\n\
                                \IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE\n\
                                \DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE\n\
                                \FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL\n\
                                \DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR\n\
                                \SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER\n\
                                \CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,\n\
                                \OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE\n\
                                \OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.\n"
licenseText MIT               = "MIT License\n\
                                \\n\
                                \Permission is hereby granted, free of charge, to any person obtaining a copy\n\
                                \of this software and associated documentation files (the \"Software\"), to deal\n\
                                \in the Software without restriction, including without limitation the rights\n\
                                \to use, copy, modify, merge, publish, distribute, sublicense, and/or sell\n\
                                \copies of the Software, and to permit persons to whom the Software is\n\
                                \furnished to do so, subject to the following conditions:\n\
                                \\n\
                                \The above copyright notice and this permission notice shall be included in all\n\
                                \copies or substantial portions of the Software.\n\
                                \\n\
                                \THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR\n\
                                \IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,\n\
                                \FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE\n\
                                \AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER\n\
                                \LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,\n\
                                \OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE\n\
                                \SOFTWARE.\n"
licenseText MPL2              = "Mozilla Public License Version 2.0\n\
                                \==================================\n\
                                \\n\
                                \1. Definitions\n\
                                \--------------\n\
                                \\n\
                                \1.1. \"Contributor\"\n\
                                \    means each individual or legal entity that creates, contributes to\n\
                                \    the creation of, or owns Covered Software.\n\
                                \\n\
                                \1.2. \"Contributor Version\"\n\
                                \    means the combination of the Contributions of others (if any) used\n\
                                \    by a Contributor and that particular Contributor's Contribution.\n\
                                \\n\
                                \1.3. \"Contribution\"\n\
                                \    means Covered Software of a particular Contributor.\n\
                                \\n\
                                \1.4. \"Covered Software\"\n\
                                \    means Source Code Form to which the initial Contributor has attached\n\
                                \    the notice in Exhibit A, the Executable Form of such Source Code\n\
                                \    Form, and Modifications of such Source Code Form, in each case\n\
                                \    including portions thereof.\n\
                                \\n\
                                \1.5. \"Incompatible With Secondary Licenses\"\n\
                                \    means\n\
                                \\n\
                                \    (a) that the initial Contributor has attached the notice described\n\
                                \        in Exhibit B to the Covered Software; or\n\
                                \\n\
                                \    (b) that the Covered Software was made available under the terms of\n\
                                \        version 1.1 or earlier of the License, but not also under the\n\
                                \        terms of a Secondary License.\n\
                                \\n\
                                \1.6. \"Executable Form\"\n\
                                \    means any form of the work other than Source Code Form.\n\
                                \\n\
                                \1.7. \"Larger Work\"\n\
                                \    means a work that combines Covered Software with other material, in \n\
                                \    a separate file or files, that is not Covered Software.\n\
                                \\n\
                                \1.8. \"License\"\n\
                                \    means this document.\n\
                                \\n\
                                \1.9. \"Licensable\"\n\
                                \    means having the right to grant, to the maximum extent possible,\n\
                                \    whether at the time of the initial grant or subsequently, any and\n\
                                \    all of the rights conveyed by this License.\n\
                                \\n\
                                \1.10. \"Modifications\"\n\
                                \    means any of the following:\n\
                                \\n\
                                \    (a) any file in Source Code Form that results from an addition to,\n\
                                \        deletion from, or modification of the contents of Covered\n\
                                \        Software; or\n\
                                \\n\
                                \    (b) any new file in Source Code Form that contains any Covered\n\
                                \        Software.\n\
                                \\n\
                                \1.11. \"Patent Claims\" of a Contributor\n\
                                \    means any patent claim(s), including without limitation, method,\n\
                                \    process, and apparatus claims, in any patent Licensable by such\n\
                                \    Contributor that would be infringed, but for the grant of the\n\
                                \    License, by the making, using, selling, offering for sale, having\n\
                                \    made, import, or transfer of either its Contributions or its\n\
                                \    Contributor Version.\n\
                                \\n\
                                \1.12. \"Secondary License\"\n\
                                \    means either the GNU General Public License, Version 2.0, the GNU\n\
                                \    Lesser General Public License, Version 2.1, the GNU Affero General\n\
                                \    Public License, Version 3.0, or any later versions of those\n\
                                \    licenses.\n\
                                \\n\
                                \1.13. \"Source Code Form\"\n\
                                \    means the form of the work preferred for making modifications.\n\
                                \\n\
                                \1.14. \"You\" (or \"Your\")\n\
                                \    means an individual or a legal entity exercising rights under this\n\
                                \    License. For legal entities, \"You\" includes any entity that\n\
                                \    controls, is controlled by, or is under common control with You. For\n\
                                \    purposes of this definition, \"control\" means (a) the power, direct\n\
                                \    or indirect, to cause the direction or management of such entity,\n\
                                \    whether by contract or otherwise, or (b) ownership of more than\n\
                                \    fifty percent (50%) of the outstanding shares or beneficial\n\
                                \    ownership of such entity.\n\
                                \\n\
                                \2. License Grants and Conditions\n\
                                \--------------------------------\n\
                                \\n\
                                \2.1. Grants\n\
                                \\n\
                                \Each Contributor hereby grants You a world-wide, royalty-free,\n\
                                \non-exclusive license:\n\
                                \\n\
                                \(a) under intellectual property rights (other than patent or trademark)\n\
                                \    Licensable by such Contributor to use, reproduce, make available,\n\
                                \    modify, display, perform, distribute, and otherwise exploit its\n\
                                \    Contributions, either on an unmodified basis, with Modifications, or\n\
                                \    as part of a Larger Work; and\n\
                                \\n\
                                \(b) under Patent Claims of such Contributor to make, use, sell, offer\n\
                                \    for sale, have made, import, and otherwise transfer either its\n\
                                \    Contributions or its Contributor Version.\n\
                                \\n\
                                \2.2. Effective Date\n\
                                \\n\
                                \The licenses granted in Section 2.1 with respect to any Contribution\n\
                                \become effective for each Contribution on the date the Contributor first\n\
                                \distributes such Contribution.\n\
                                \\n\
                                \2.3. Limitations on Grant Scope\n\
                                \\n\
                                \The licenses granted in this Section 2 are the only rights granted under\n\
                                \this License. No additional rights or licenses will be implied from the\n\
                                \distribution or licensing of Covered Software under this License.\n\
                                \Notwithstanding Section 2.1(b) above, no patent license is granted by a\n\
                                \Contributor:\n\
                                \\n\
                                \(a) for any code that a Contributor has removed from Covered Software;\n\
                                \    or\n\
                                \\n\
                                \(b) for infringements caused by: (i) Your and any other third party's\n\
                                \    modifications of Covered Software, or (ii) the combination of its\n\
                                \    Contributions with other software (except as part of its Contributor\n\
                                \    Version); or\n\
                                \\n\
                                \(c) under Patent Claims infringed by Covered Software in the absence of\n\
                                \    its Contributions.\n\
                                \\n\
                                \This License does not grant any rights in the trademarks, service marks,\n\
                                \or logos of any Contributor (except as may be necessary to comply with\n\
                                \the notice requirements in Section 3.4).\n\
                                \\n\
                                \2.4. Subsequent Licenses\n\
                                \\n\
                                \No Contributor makes additional grants as a result of Your choice to\n\
                                \distribute the Covered Software under a subsequent version of this\n\
                                \License (see Section 10.2) or under the terms of a Secondary License (if\n\
                                \permitted under the terms of Section 3.3).\n\
                                \\n\
                                \2.5. Representation\n\
                                \\n\
                                \Each Contributor represents that the Contributor believes its\n\
                                \Contributions are its original creation(s) or it has sufficient rights\n\
                                \to grant the rights to its Contributions conveyed by this License.\n\
                                \\n\
                                \2.6. Fair Use\n\
                                \\n\
                                \This License is not intended to limit any rights You have under\n\
                                \applicable copyright doctrines of fair use, fair dealing, or other\n\
                                \equivalents.\n\
                                \\n\
                                \2.7. Conditions\n\
                                \\n\
                                \Sections 3.1, 3.2, 3.3, and 3.4 are conditions of the licenses granted\n\
                                \in Section 2.1.\n\
                                \\n\
                                \3. Responsibilities\n\
                                \-------------------\n\
                                \\n\
                                \3.1. Distribution of Source Form\n\
                                \\n\
                                \All distribution of Covered Software in Source Code Form, including any\n\
                                \Modifications that You create or to which You contribute, must be under\n\
                                \the terms of this License. You must inform recipients that the Source\n\
                                \Code Form of the Covered Software is governed by the terms of this\n\
                                \License, and how they can obtain a copy of this License. You may not\n\
                                \attempt to alter or restrict the recipients' rights in the Source Code\n\
                                \Form.\n\
                                \\n\
                                \3.2. Distribution of Executable Form\n\
                                \\n\
                                \If You distribute Covered Software in Executable Form then:\n\
                                \\n\
                                \(a) such Covered Software must also be made available in Source Code\n\
                                \    Form, as described in Section 3.1, and You must inform recipients of\n\
                                \    the Executable Form how they can obtain a copy of such Source Code\n\
                                \    Form by reasonable means in a timely manner, at a charge no more\n\
                                \    than the cost of distribution to the recipient; and\n\
                                \\n\
                                \(b) You may distribute such Executable Form under the terms of this\n\
                                \    License, or sublicense it under different terms, provided that the\n\
                                \    license for the Executable Form does not attempt to limit or alter\n\
                                \    the recipients' rights in the Source Code Form under this License.\n\
                                \\n\
                                \3.3. Distribution of a Larger Work\n\
                                \\n\
                                \You may create and distribute a Larger Work under terms of Your choice,\n\
                                \provided that You also comply with the requirements of this License for\n\
                                \the Covered Software. If the Larger Work is a combination of Covered\n\
                                \Software with a work governed by one or more Secondary Licenses, and the\n\
                                \Covered Software is not Incompatible With Secondary Licenses, this\n\
                                \License permits You to additionally distribute such Covered Software\n\
                                \under the terms of such Secondary License(s), so that the recipient of\n\
                                \the Larger Work may, at their option, further distribute the Covered\n\
                                \Software under the terms of either this License or such Secondary\n\
                                \License(s).\n\
                                \\n\
                                \3.4. Notices\n\
                                \\n\
                                \You may not remove or alter the substance of any license notices\n\
                                \(including copyright notices, patent notices, disclaimers of warranty,\n\
                                \or limitations of liability) contained within the Source Code Form of\n\
                                \the Covered Software, except that You may alter any license notices to\n\
                                \the extent required to remedy known factual inaccuracies.\n\
                                \\n\
                                \3.5. Application of Additional Terms\n\
                                \\n\
                                \You may choose to offer, and to charge a fee for, warranty, support,\n\
                                \indemnity or liability obligations to one or more recipients of Covered\n\
                                \Software. However, You may do so only on Your own behalf, and not on\n\
                                \behalf of any Contributor. You must make it absolutely clear that any\n\
                                \such warranty, support, indemnity, or liability obligation is offered by\n\
                                \You alone, and You hereby agree to indemnify every Contributor for any\n\
                                \liability incurred by such Contributor as a result of warranty, support,\n\
                                \indemnity or liability terms You offer. You may include additional\n\
                                \disclaimers of warranty and limitations of liability specific to any\n\
                                \jurisdiction.\n\
                                \\n\
                                \4. Inability to Comply Due to Statute or Regulation\n\
                                \---------------------------------------------------\n\
                                \\n\
                                \If it is impossible for You to comply with any of the terms of this\n\
                                \License with respect to some or all of the Covered Software due to\n\
                                \statute, judicial order, or regulation then You must: (a) comply with\n\
                                \the terms of this License to the maximum extent possible; and (b)\n\
                                \describe the limitations and the code they affect. Such description must\n\
                                \be placed in a text file included with all distributions of the Covered\n\
                                \Software under this License. Except to the extent prohibited by statute\n\
                                \or regulation, such description must be sufficiently detailed for a\n\
                                \recipient of ordinary skill to be able to understand it.\n\
                                \\n\
                                \5. Termination\n\
                                \--------------\n\
                                \\n\
                                \5.1. The rights granted under this License will terminate automatically\n\
                                \if You fail to comply with any of its terms. However, if You become\n\
                                \compliant, then the rights granted under this License from a particular\n\
                                \Contributor are reinstated (a) provisionally, unless and until such\n\
                                \Contributor explicitly and finally terminates Your grants, and (b) on an\n\
                                \ongoing basis, if such Contributor fails to notify You of the\n\
                                \non-compliance by some reasonable means prior to 60 days after You have\n\
                                \come back into compliance. Moreover, Your grants from a particular\n\
                                \Contributor are reinstated on an ongoing basis if such Contributor\n\
                                \notifies You of the non-compliance by some reasonable means, this is the\n\
                                \first time You have received notice of non-compliance with this License\n\
                                \from such Contributor, and You become compliant prior to 30 days after\n\
                                \Your receipt of the notice.\n\
                                \\n\
                                \5.2. If You initiate litigation against any entity by asserting a patent\n\
                                \infringement claim (excluding declaratory judgment actions,\n\
                                \counter-claims, and cross-claims) alleging that a Contributor Version\n\
                                \directly or indirectly infringes any patent, then the rights granted to\n\
                                \You by any and all Contributors for the Covered Software under Section\n\
                                \2.1 of this License shall terminate.\n\
                                \\n\
                                \5.3. In the event of termination under Sections 5.1 or 5.2 above, all\n\
                                \end user license agreements (excluding distributors and resellers) which\n\
                                \have been validly granted by You or Your distributors under this License\n\
                                \prior to termination shall survive termination.\n\
                                \\n\
                                \************************************************************************\n\
                                \*                                                                      *\n\
                                \*  6. Disclaimer of Warranty                                           *\n\
                                \*  -------------------------                                           *\n\
                                \*                                                                      *\n\
                                \*  Covered Software is provided under this License on an \"as is\"       *\n\
                                \*  basis, without warranty of any kind, either expressed, implied, or  *\n\
                                \*  statutory, including, without limitation, warranties that the       *\n\
                                \*  Covered Software is free of defects, merchantable, fit for a        *\n\
                                \*  particular purpose or non-infringing. The entire risk as to the     *\n\
                                \*  quality and performance of the Covered Software is with You.        *\n\
                                \*  Should any Covered Software prove defective in any respect, You     *\n\
                                \*  (not any Contributor) assume the cost of any necessary servicing,   *\n\
                                \*  repair, or correction. This disclaimer of warranty constitutes an   *\n\
                                \*  essential part of this License. No use of any Covered Software is   *\n\
                                \*  authorized under this License except under this disclaimer.         *\n\
                                \*                                                                      *\n\
                                \************************************************************************\n\
                                \\n\
                                \************************************************************************\n\
                                \*                                                                      *\n\
                                \*  7. Limitation of Liability                                          *\n\
                                \*  --------------------------                                          *\n\
                                \*                                                                      *\n\
                                \*  Under no circumstances and under no legal theory, whether tort      *\n\
                                \*  (including negligence), contract, or otherwise, shall any           *\n\
                                \*  Contributor, or anyone who distributes Covered Software as          *\n\
                                \*  permitted above, be liable to You for any direct, indirect,         *\n\
                                \*  special, incidental, or consequential damages of any character      *\n\
                                \*  including, without limitation, damages for lost profits, loss of    *\n\
                                \*  goodwill, work stoppage, computer failure or malfunction, or any    *\n\
                                \*  and all other commercial damages or losses, even if such party      *\n\
                                \*  shall have been informed of the possibility of such damages. This   *\n\
                                \*  limitation of liability shall not apply to liability for death or   *\n\
                                \*  personal injury resulting from such party's negligence to the       *\n\
                                \*  extent applicable law prohibits such limitation. Some               *\n\
                                \*  jurisdictions do not allow the exclusion or limitation of           *\n\
                                \*  incidental or consequential damages, so this exclusion and          *\n\
                                \*  limitation may not apply to You.                                    *\n\
                                \*                                                                      *\n\
                                \************************************************************************\n\
                                \\n\
                                \8. Litigation\n\
                                \-------------\n\
                                \\n\
                                \Any litigation relating to this License may be brought only in the\n\
                                \courts of a jurisdiction where the defendant maintains its principal\n\
                                \place of business and such litigation shall be governed by laws of that\n\
                                \jurisdiction, without reference to its conflict-of-law provisions.\n\
                                \Nothing in this Section shall prevent a party's ability to bring\n\
                                \cross-claims or counter-claims.\n\
                                \\n\
                                \9. Miscellaneous\n\
                                \----------------\n\
                                \\n\
                                \This License represents the complete agreement concerning the subject\n\
                                \matter hereof. If any provision of this License is held to be\n\
                                \unenforceable, such provision shall be reformed only to the extent\n\
                                \necessary to make it enforceable. Any law or regulation which provides\n\
                                \that the language of a contract shall be construed against the drafter\n\
                                \shall not be used to construe this License against a Contributor.\n\
                                \\n\
                                \10. Versions of the License\n\
                                \---------------------------\n\
                                \\n\
                                \10.1. New Versions\n\
                                \\n\
                                \Mozilla Foundation is the license steward. Except as provided in Section\n\
                                \10.3, no one other than the license steward has the right to modify or\n\
                                \publish new versions of this License. Each version will be given a\n\
                                \distinguishing version number.\n\
                                \\n\
                                \10.2. Effect of New Versions\n\
                                \\n\
                                \You may distribute the Covered Software under the terms of the version\n\
                                \of the License under which You originally received the Covered Software,\n\
                                \or under the terms of any subsequent version published by the license\n\
                                \steward.\n\
                                \\n\
                                \10.3. Modified Versions\n\
                                \\n\
                                \If you create software not governed by this License, and you want to\n\
                                \create a new license for such software, you may create and use a\n\
                                \modified version of this License if you rename the license and remove\n\
                                \any references to the name of the license steward (except to note that\n\
                                \such modified license differs from this License).\n\
                                \\n\
                                \10.4. Distributing Source Code Form that is Incompatible With Secondary\n\
                                \Licenses\n\
                                \\n\
                                \If You choose to distribute Source Code Form that is Incompatible With\n\
                                \Secondary Licenses under the terms of this version of the License, the\n\
                                \notice described in Exhibit B of this License must be attached.\n\
                                \\n\
                                \Exhibit A - Source Code Form License Notice\n\
                                \-------------------------------------------\n\
                                \\n\
                                \  This Source Code Form is subject to the terms of the Mozilla Public\n\
                                \  License, v. 2.0. If a copy of the MPL was not distributed with this\n\
                                \  file, You can obtain one at http://mozilla.org/MPL/2.0/.\n\
                                \\n\
                                \If it is not possible or desirable to put the notice in a particular\n\
                                \file, then You may include the notice in a location (such as a LICENSE\n\
                                \file in a relevant directory) where a recipient would be likely to look\n\
                                \for such a notice.\n\
                                \\n\
                                \You may add additional accurate notices of copyright ownership.\n\
                                \\n\
                                \Exhibit B - \"Incompatible With Secondary Licenses\" Notice\n\
                                \---------------------------------------------------------\n\
                                \\n\
                                \  This Source Code Form is \"Incompatible With Secondary Licenses\", as\n\
                                \  defined by the Mozilla Public License, v. 2.0.\n"
licenseText Apache2           = "                                 Apache License\n\
                                \                           Version 2.0, January 2004\n\
                                \                        http://www.apache.org/licenses/\n\
                                \\n\
                                \   TERMS AND CONDITIONS FOR USE, REPRODUCTION, AND DISTRIBUTION\n\
                                \\n\
                                \   1. Definitions.\n\
                                \\n\
                                \      \"License\" shall mean the terms and conditions for use, reproduction,\n\
                                \      and distribution as defined by Sections 1 through 9 of this document.\n\
                                \\n\
                                \      \"Licensor\" shall mean the copyright owner or entity authorized by\n\
                                \      the copyright owner that is granting the License.\n\
                                \\n\
                                \      \"Legal Entity\" shall mean the union of the acting entity and all\n\
                                \      other entities that control, are controlled by, or are under common\n\
                                \      control with that entity. For the purposes of this definition,\n\
                                \      \"control\" means (i) the power, direct or indirect, to cause the\n\
                                \      direction or management of such entity, whether by contract or\n\
                                \      otherwise, or (ii) ownership of fifty percent (50%) or more of the\n\
                                \      outstanding shares, or (iii) beneficial ownership of such entity.\n\
                                \\n\
                                \      \"You\" (or \"Your\") shall mean an individual or Legal Entity\n\
                                \      exercising permissions granted by this License.\n\
                                \\n\
                                \      \"Source\" form shall mean the preferred form for making modifications,\n\
                                \      including but not limited to software source code, documentation\n\
                                \      source, and configuration files.\n\
                                \\n\
                                \      \"Object\" form shall mean any form resulting from mechanical\n\
                                \      transformation or translation of a Source form, including but\n\
                                \      not limited to compiled object code, generated documentation,\n\
                                \      and conversions to other media types.\n\
                                \\n\
                                \      \"Work\" shall mean the work of authorship, whether in Source or\n\
                                \      Object form, made available under the License, as indicated by a\n\
                                \      copyright notice that is included in or attached to the work\n\
                                \      (an example is provided in the Appendix below).\n\
                                \\n\
                                \      \"Derivative Works\" shall mean any work, whether in Source or Object\n\
                                \      form, that is based on (or derived from) the Work and for which the\n\
                                \      editorial revisions, annotations, elaborations, or other modifications\n\
                                \      represent, as a whole, an original work of authorship. For the purposes\n\
                                \      of this License, Derivative Works shall not include works that remain\n\
                                \      separable from, or merely link (or bind by name) to the interfaces of,\n\
                                \      the Work and Derivative Works thereof.\n\
                                \\n\
                                \      \"Contribution\" shall mean any work of authorship, including\n\
                                \      the original version of the Work and any modifications or additions\n\
                                \      to that Work or Derivative Works thereof, that is intentionally\n\
                                \      submitted to Licensor for inclusion in the Work by the copyright owner\n\
                                \      or by an individual or Legal Entity authorized to submit on behalf of\n\
                                \      the copyright owner. For the purposes of this definition, \"submitted\"\n\
                                \      means any form of electronic, verbal, or written communication sent\n\
                                \      to the Licensor or its representatives, including but not limited to\n\
                                \      communication on electronic mailing lists, source code control systems,\n\
                                \      and issue tracking systems that are managed by, or on behalf of, the\n\
                                \      Licensor for the purpose of discussing and improving the Work, but\n\
                                \      excluding communication that is conspicuously marked or otherwise\n\
                                \      designated in writing by the copyright owner as \"Not a Contribution.\"\n\
                                \\n\
                                \      \"Contributor\" shall mean Licensor and any individual or Legal Entity\n\
                                \      on behalf of whom a Contribution has been received by Licensor and\n\
                                \      subsequently incorporated within the Work.\n\
                                \\n\
                                \   2. Grant of Copyright License. Subject to the terms and conditions of\n\
                                \      this License, each Contributor hereby grants to You a perpetual,\n\
                                \      worldwide, non-exclusive, no-charge, royalty-free, irrevocable\n\
                                \      copyright license to reproduce, prepare Derivative Works of,\n\
                                \      publicly display, publicly perform, sublicense, and distribute the\n\
                                \      Work and such Derivative Works in Source or Object form.\n\
                                \\n\
                                \   3. Grant of Patent License. Subject to the terms and conditions of\n\
                                \      this License, each Contributor hereby grants to You a perpetual,\n\
                                \      worldwide, non-exclusive, no-charge, royalty-free, irrevocable\n\
                                \      (except as stated in this section) patent license to make, have made,\n\
                                \      use, offer to sell, sell, import, and otherwise transfer the Work,\n\
                                \      where such license applies only to those patent claims licensable\n\
                                \      by such Contributor that are necessarily infringed by their\n\
                                \      Contribution(s) alone or by combination of their Contribution(s)\n\
                                \      with the Work to which such Contribution(s) was submitted. If You\n\
                                \      institute patent litigation against any entity (including a\n\
                                \      cross-claim or counterclaim in a lawsuit) alleging that the Work\n\
                                \      or a Contribution incorporated within the Work constitutes direct\n\
                                \      or contributory patent infringement, then any patent licenses\n\
                                \      granted to You under this License for that Work shall terminate\n\
                                \      as of the date such litigation is filed.\n\
                                \\n\
                                \   4. Redistribution. You may reproduce and distribute copies of the\n\
                                \      Work or Derivative Works thereof in any medium, with or without\n\
                                \      modifications, and in Source or Object form, provided that You\n\
                                \      meet the following conditions:\n\
                                \\n\
                                \      (a) You must give any other recipients of the Work or\n\
                                \          Derivative Works a copy of this License; and\n\
                                \\n\
                                \      (b) You must cause any modified files to carry prominent notices\n\
                                \          stating that You changed the files; and\n\
                                \\n\
                                \      (c) You must retain, in the Source form of any Derivative Works\n\
                                \          that You distribute, all copyright, patent, trademark, and\n\
                                \          attribution notices from the Source form of the Work,\n\
                                \          excluding those notices that do not pertain to any part of\n\
                                \          the Derivative Works; and\n\
                                \\n\
                                \      (d) If the Work includes a \"NOTICE\" text file as part of its\n\
                                \          distribution, then any Derivative Works that You distribute must\n\
                                \          include a readable copy of the attribution notices contained\n\
                                \          within such NOTICE file, excluding those notices that do not\n\
                                \          pertain to any part of the Derivative Works, in at least one\n\
                                \          of the following places: within a NOTICE text file distributed\n\
                                \          as part of the Derivative Works; within the Source form or\n\
                                \          documentation, if provided along with the Derivative Works; or,\n\
                                \          within a display generated by the Derivative Works, if and\n\
                                \          wherever such third-party notices normally appear. The contents\n\
                                \          of the NOTICE file are for informational purposes only and\n\
                                \          do not modify the License. You may add Your own attribution\n\
                                \          notices within Derivative Works that You distribute, alongside\n\
                                \          or as an addendum to the NOTICE text from the Work, provided\n\
                                \          that such additional attribution notices cannot be construed\n\
                                \          as modifying the License.\n\
                                \\n\
                                \      You may add Your own copyright statement to Your modifications and\n\
                                \      may provide additional or different license terms and conditions\n\
                                \      for use, reproduction, or distribution of Your modifications, or\n\
                                \      for any such Derivative Works as a whole, provided Your use,\n\
                                \      reproduction, and distribution of the Work otherwise complies with\n\
                                \      the conditions stated in this License.\n\
                                \\n\
                                \   5. Submission of Contributions. Unless You explicitly state otherwise,\n\
                                \      any Contribution intentionally submitted for inclusion in the Work\n\
                                \      by You to the Licensor shall be under the terms and conditions of\n\
                                \      this License, without any additional terms or conditions.\n\
                                \      Notwithstanding the above, nothing herein shall supersede or modify\n\
                                \      the terms of any separate license agreement you may have executed\n\
                                \      with Licensor regarding such Contributions.\n\
                                \\n\
                                \   6. Trademarks. This License does not grant permission to use the trade\n\
                                \      names, trademarks, service marks, or product names of the Licensor,\n\
                                \      except as required for reasonable and customary use in describing the\n\
                                \      origin of the Work and reproducing the content of the NOTICE file.\n\
                                \\n\
                                \   7. Disclaimer of Warranty. Unless required by applicable law or\n\
                                \      agreed to in writing, Licensor provides the Work (and each\n\
                                \      Contributor provides its Contributions) on an \"AS IS\" BASIS,\n\
                                \      WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or\n\
                                \      implied, including, without limitation, any warranties or conditions\n\
                                \      of TITLE, NON-INFRINGEMENT, MERCHANTABILITY, or FITNESS FOR A\n\
                                \      PARTICULAR PURPOSE. You are solely responsible for determining the\n\
                                \      appropriateness of using or redistributing the Work and assume any\n\
                                \      risks associated with Your exercise of permissions under this License.\n\
                                \\n\
                                \   8. Limitation of Liability. In no event and under no legal theory,\n\
                                \      whether in tort (including negligence), contract, or otherwise,\n\
                                \      unless required by applicable law (such as deliberate and grossly\n\
                                \      negligent acts) or agreed to in writing, shall any Contributor be\n\
                                \      liable to You for damages, including any direct, indirect, special,\n\
                                \      incidental, or consequential damages of any character arising as a\n\
                                \      result of this License or out of the use or inability to use the\n\
                                \      Work (including but not limited to damages for loss of goodwill,\n\
                                \      work stoppage, computer failure or malfunction, or any and all\n\
                                \      other commercial damages or losses), even if such Contributor\n\
                                \      has been advised of the possibility of such damages.\n\
                                \\n\
                                \   9. Accepting Warranty or Additional Liability. While redistributing\n\
                                \      the Work or Derivative Works thereof, You may choose to offer,\n\
                                \      and charge a fee for, acceptance of support, warranty, indemnity,\n\
                                \      or other liability obligations and/or rights consistent with this\n\
                                \      License. However, in accepting such obligations, You may act only\n\
                                \      on Your own behalf and on Your sole responsibility, not on behalf\n\
                                \      of any other Contributor, and only if You agree to indemnify,\n\
                                \      defend, and hold each Contributor harmless for any liability\n\
                                \      incurred by, or claims asserted against, such Contributor by reason\n\
                                \      of your accepting any such warranty or additional liability.\n\
                                \\n\
                                \   END OF TERMS AND CONDITIONS\n\
                                \\n\
                                \   APPENDIX: How to apply the Apache License to your work.\n\
                                \\n\
                                \      To apply the Apache License to your work, attach the following\n\
                                \      boilerplate notice, with the fields enclosed by brackets \"[]\"\n\
                                \      replaced with your own identifying information. (Don't include\n\
                                \      the brackets!)  The text should be enclosed in the appropriate\n\
                                \      comment syntax for the file format. We also recommend that a\n\
                                \      file or class name and description of purpose be included on the\n\
                                \      same \"printed page\" as the copyright notice for easier\n\
                                \      identification within third-party archives.\n\
                                \\n\
                                \   Copyright [yyyy] [name of copyright owner]\n\
                                \\n\
                                \   Licensed under the Apache License, Version 2.0 (the \"License\");\n\
                                \   you may not use this file except in compliance with the License.\n\
                                \   You may obtain a copy of the License at\n\
                                \\n\
                                \       http://www.apache.org/licenses/LICENSE-2.0\n\
                                \\n\
                                \   Unless required by applicable law or agreed to in writing, software\n\
                                \   distributed under the License is distributed on an \"AS IS\" BASIS,\n\
                                \   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.\n\
                                \   See the License for the specific language governing permissions and\n\
                                \   limitations under the License.\n"
licenseText PublicDomain      = "This is free and unencumbered software released into the public domain.\n\
                                \\n\
                                \Anyone is free to copy, modify, publish, use, compile, sell, or\n\
                                \distribute this software, either in source code form or as a compiled\n\
                                \binary, for any purpose, commercial or non-commercial, and by any\n\
                                \means.\n\
                                \\n\
                                \In jurisdictions that recognize copyright laws, the author or authors\n\
                                \of this software dedicate any and all copyright interest in the\n\
                                \software to the public domain. We make this dedication for the benefit\n\
                                \of the public at large and to the detriment of our heirs and\n\
                                \successors. We intend this dedication to be an overt act of\n\
                                \relinquishment in perpetuity of all present and future rights to this\n\
                                \software under copyright law.\n\
                                \\n\
                                \THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND,\n\
                                \EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF\n\
                                \MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.\n\
                                \IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR\n\
                                \OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,\n\
                                \ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR\n\
                                \OTHER DEALINGS IN THE SOFTWARE.\n\
                                \\n\
                                \For more information, please refer to <http://unlicense.org/>\n"
licenseText AllRightsReserved = ""
licenseText Other             = ""

parseLicense :: M.Parser License
parseLicense = M.choice lics
    where lics = [ lexed (M.string "GPL2" *> pure GPL2)
                 , lexed (M.string "GPL3" *> pure GPL3)
                 , lexed (M.string "AGPL" *> pure AGPL)
                 , lexed (M.string "LGPL2.1" *> pure LGPL21)
                 , lexed (M.string "LGPL3" *> pure LGPL3)
                 , lexed (M.string "BSD2" *> pure BSD2)
                 , lexed (M.string "BSD3" *> pure BSD3)
                 , lexed (M.string "MIT" *> pure MIT)
                 , lexed (M.string "MPL2" *> pure MPL2)
                 , lexed (M.string "Apache2" *> pure MPL2)
                 , lexed (M.string "PublicDomain" *> pure PublicDomain)
                 , lexed (M.string "AllRightsReserved" *> pure AllRightsReserved)
                 , lexed (M.string "Other" *> pure Other)
                 ]
