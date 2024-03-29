-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql
module Taco.Enum.MimeTypeEnum exposing (..)

import Json.Decode as Decode exposing (Decoder)


{-| The MimeType of the object
-}
type MimeTypeEnum
    = ApplicationJava
    | ApplicationMsword
    | ApplicationOctetStream
    | ApplicationOnenote
    | ApplicationOxps
    | ApplicationPdf
    | ApplicationRar
    | ApplicationRtf
    | ApplicationTtafXml
    | ApplicationVndAppleKeynote
    | ApplicationVndAppleNumbers
    | ApplicationVndApplePages
    | ApplicationVndMsAccess
    | ApplicationVndMsExcel
    | ApplicationVndMsExcelAddinMacroenabled12
    | ApplicationVndMsExcelSheetBinaryMacroenabled12
    | ApplicationVndMsExcelSheetMacroenabled12
    | ApplicationVndMsExcelTemplateMacroenabled12
    | ApplicationVndMsPowerpoint
    | ApplicationVndMsPowerpointAddinMacroenabled12
    | ApplicationVndMsPowerpointPresentationMacroenabled12
    | ApplicationVndMsPowerpointSlideshowMacroenabled12
    | ApplicationVndMsPowerpointSlideMacroenabled12
    | ApplicationVndMsPowerpointTemplateMacroenabled12
    | ApplicationVndMsProject
    | ApplicationVndMsWordDocumentMacroenabled12
    | ApplicationVndMsWordTemplateMacroenabled12
    | ApplicationVndMsWrite
    | ApplicationVndMsXpsdocument
    | ApplicationVndOasisOpendocumentChart
    | ApplicationVndOasisOpendocumentDatabase
    | ApplicationVndOasisOpendocumentFormula
    | ApplicationVndOasisOpendocumentGraphics
    | ApplicationVndOasisOpendocumentPresentation
    | ApplicationVndOasisOpendocumentSpreadsheet
    | ApplicationVndOasisOpendocumentText
    | ApplicationVndOpenxmlformatsOfficedocumentPresentationmlPresentation
    | ApplicationVndOpenxmlformatsOfficedocumentPresentationmlSlide
    | ApplicationVndOpenxmlformatsOfficedocumentPresentationmlSlideshow
    | ApplicationVndOpenxmlformatsOfficedocumentPresentationmlTemplate
    | ApplicationVndOpenxmlformatsOfficedocumentSpreadsheetmlSheet
    | ApplicationVndOpenxmlformatsOfficedocumentSpreadsheetmlTemplate
    | ApplicationVndOpenxmlformatsOfficedocumentWordprocessingmlDocument
    | ApplicationVndOpenxmlformatsOfficedocumentWordprocessingmlTemplate
    | ApplicationWordperfect
    | ApplicationX7zCompressed
    | ApplicationXGzip
    | ApplicationXTar
    | ApplicationZip
    | AudioAac
    | AudioFlac
    | AudioMidi
    | AudioMpeg
    | AudioOgg
    | AudioWav
    | AudioXMatroska
    | AudioXMsWax
    | AudioXMsWma
    | AudioXRealaudio
    | ImageBmp
    | ImageGif
    | ImageHeic
    | ImageJpeg
    | ImagePng
    | ImageTiff
    | ImageXIcon
    | TextCalendar
    | TextCss
    | TextCsv
    | TextPlain
    | TextRichtext
    | TextTabSeparatedValues
    | TextVtt
    | Video3gpp
    | Video3gpp2
    | VideoAvi
    | VideoDivx
    | VideoMp4
    | VideoMpeg
    | VideoOgg
    | VideoQuicktime
    | VideoWebm
    | VideoXFlv
    | VideoXMatroska
    | VideoXMsAsf
    | VideoXMsWm
    | VideoXMsWmv
    | VideoXMsWmx
list : List MimeTypeEnum
list =
    [ApplicationJava, ApplicationMsword, ApplicationOctetStream, ApplicationOnenote, ApplicationOxps, ApplicationPdf, ApplicationRar, ApplicationRtf, ApplicationTtafXml, ApplicationVndAppleKeynote, ApplicationVndAppleNumbers, ApplicationVndApplePages, ApplicationVndMsAccess, ApplicationVndMsExcel, ApplicationVndMsExcelAddinMacroenabled12, ApplicationVndMsExcelSheetBinaryMacroenabled12, ApplicationVndMsExcelSheetMacroenabled12, ApplicationVndMsExcelTemplateMacroenabled12, ApplicationVndMsPowerpoint, ApplicationVndMsPowerpointAddinMacroenabled12, ApplicationVndMsPowerpointPresentationMacroenabled12, ApplicationVndMsPowerpointSlideshowMacroenabled12, ApplicationVndMsPowerpointSlideMacroenabled12, ApplicationVndMsPowerpointTemplateMacroenabled12, ApplicationVndMsProject, ApplicationVndMsWordDocumentMacroenabled12, ApplicationVndMsWordTemplateMacroenabled12, ApplicationVndMsWrite, ApplicationVndMsXpsdocument, ApplicationVndOasisOpendocumentChart, ApplicationVndOasisOpendocumentDatabase, ApplicationVndOasisOpendocumentFormula, ApplicationVndOasisOpendocumentGraphics, ApplicationVndOasisOpendocumentPresentation, ApplicationVndOasisOpendocumentSpreadsheet, ApplicationVndOasisOpendocumentText, ApplicationVndOpenxmlformatsOfficedocumentPresentationmlPresentation, ApplicationVndOpenxmlformatsOfficedocumentPresentationmlSlide, ApplicationVndOpenxmlformatsOfficedocumentPresentationmlSlideshow, ApplicationVndOpenxmlformatsOfficedocumentPresentationmlTemplate, ApplicationVndOpenxmlformatsOfficedocumentSpreadsheetmlSheet, ApplicationVndOpenxmlformatsOfficedocumentSpreadsheetmlTemplate, ApplicationVndOpenxmlformatsOfficedocumentWordprocessingmlDocument, ApplicationVndOpenxmlformatsOfficedocumentWordprocessingmlTemplate, ApplicationWordperfect, ApplicationX7zCompressed, ApplicationXGzip, ApplicationXTar, ApplicationZip, AudioAac, AudioFlac, AudioMidi, AudioMpeg, AudioOgg, AudioWav, AudioXMatroska, AudioXMsWax, AudioXMsWma, AudioXRealaudio, ImageBmp, ImageGif, ImageHeic, ImageJpeg, ImagePng, ImageTiff, ImageXIcon, TextCalendar, TextCss, TextCsv, TextPlain, TextRichtext, TextTabSeparatedValues, TextVtt, Video3gpp, Video3gpp2, VideoAvi, VideoDivx, VideoMp4, VideoMpeg, VideoOgg, VideoQuicktime, VideoWebm, VideoXFlv, VideoXMatroska, VideoXMsAsf, VideoXMsWm, VideoXMsWmv, VideoXMsWmx]
decoder : Decoder MimeTypeEnum
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "APPLICATION_JAVA" ->
                        Decode.succeed ApplicationJava

                    "APPLICATION_MSWORD" ->
                        Decode.succeed ApplicationMsword

                    "APPLICATION_OCTET_STREAM" ->
                        Decode.succeed ApplicationOctetStream

                    "APPLICATION_ONENOTE" ->
                        Decode.succeed ApplicationOnenote

                    "APPLICATION_OXPS" ->
                        Decode.succeed ApplicationOxps

                    "APPLICATION_PDF" ->
                        Decode.succeed ApplicationPdf

                    "APPLICATION_RAR" ->
                        Decode.succeed ApplicationRar

                    "APPLICATION_RTF" ->
                        Decode.succeed ApplicationRtf

                    "APPLICATION_TTAF_XML" ->
                        Decode.succeed ApplicationTtafXml

                    "APPLICATION_VND_APPLE_KEYNOTE" ->
                        Decode.succeed ApplicationVndAppleKeynote

                    "APPLICATION_VND_APPLE_NUMBERS" ->
                        Decode.succeed ApplicationVndAppleNumbers

                    "APPLICATION_VND_APPLE_PAGES" ->
                        Decode.succeed ApplicationVndApplePages

                    "APPLICATION_VND_MS_ACCESS" ->
                        Decode.succeed ApplicationVndMsAccess

                    "APPLICATION_VND_MS_EXCEL" ->
                        Decode.succeed ApplicationVndMsExcel

                    "APPLICATION_VND_MS_EXCEL_ADDIN_MACROENABLED_12" ->
                        Decode.succeed ApplicationVndMsExcelAddinMacroenabled12

                    "APPLICATION_VND_MS_EXCEL_SHEET_BINARY_MACROENABLED_12" ->
                        Decode.succeed ApplicationVndMsExcelSheetBinaryMacroenabled12

                    "APPLICATION_VND_MS_EXCEL_SHEET_MACROENABLED_12" ->
                        Decode.succeed ApplicationVndMsExcelSheetMacroenabled12

                    "APPLICATION_VND_MS_EXCEL_TEMPLATE_MACROENABLED_12" ->
                        Decode.succeed ApplicationVndMsExcelTemplateMacroenabled12

                    "APPLICATION_VND_MS_POWERPOINT" ->
                        Decode.succeed ApplicationVndMsPowerpoint

                    "APPLICATION_VND_MS_POWERPOINT_ADDIN_MACROENABLED_12" ->
                        Decode.succeed ApplicationVndMsPowerpointAddinMacroenabled12

                    "APPLICATION_VND_MS_POWERPOINT_PRESENTATION_MACROENABLED_12" ->
                        Decode.succeed ApplicationVndMsPowerpointPresentationMacroenabled12

                    "APPLICATION_VND_MS_POWERPOINT_SLIDESHOW_MACROENABLED_12" ->
                        Decode.succeed ApplicationVndMsPowerpointSlideshowMacroenabled12

                    "APPLICATION_VND_MS_POWERPOINT_SLIDE_MACROENABLED_12" ->
                        Decode.succeed ApplicationVndMsPowerpointSlideMacroenabled12

                    "APPLICATION_VND_MS_POWERPOINT_TEMPLATE_MACROENABLED_12" ->
                        Decode.succeed ApplicationVndMsPowerpointTemplateMacroenabled12

                    "APPLICATION_VND_MS_PROJECT" ->
                        Decode.succeed ApplicationVndMsProject

                    "APPLICATION_VND_MS_WORD_DOCUMENT_MACROENABLED_12" ->
                        Decode.succeed ApplicationVndMsWordDocumentMacroenabled12

                    "APPLICATION_VND_MS_WORD_TEMPLATE_MACROENABLED_12" ->
                        Decode.succeed ApplicationVndMsWordTemplateMacroenabled12

                    "APPLICATION_VND_MS_WRITE" ->
                        Decode.succeed ApplicationVndMsWrite

                    "APPLICATION_VND_MS_XPSDOCUMENT" ->
                        Decode.succeed ApplicationVndMsXpsdocument

                    "APPLICATION_VND_OASIS_OPENDOCUMENT_CHART" ->
                        Decode.succeed ApplicationVndOasisOpendocumentChart

                    "APPLICATION_VND_OASIS_OPENDOCUMENT_DATABASE" ->
                        Decode.succeed ApplicationVndOasisOpendocumentDatabase

                    "APPLICATION_VND_OASIS_OPENDOCUMENT_FORMULA" ->
                        Decode.succeed ApplicationVndOasisOpendocumentFormula

                    "APPLICATION_VND_OASIS_OPENDOCUMENT_GRAPHICS" ->
                        Decode.succeed ApplicationVndOasisOpendocumentGraphics

                    "APPLICATION_VND_OASIS_OPENDOCUMENT_PRESENTATION" ->
                        Decode.succeed ApplicationVndOasisOpendocumentPresentation

                    "APPLICATION_VND_OASIS_OPENDOCUMENT_SPREADSHEET" ->
                        Decode.succeed ApplicationVndOasisOpendocumentSpreadsheet

                    "APPLICATION_VND_OASIS_OPENDOCUMENT_TEXT" ->
                        Decode.succeed ApplicationVndOasisOpendocumentText

                    "APPLICATION_VND_OPENXMLFORMATS_OFFICEDOCUMENT_PRESENTATIONML_PRESENTATION" ->
                        Decode.succeed ApplicationVndOpenxmlformatsOfficedocumentPresentationmlPresentation

                    "APPLICATION_VND_OPENXMLFORMATS_OFFICEDOCUMENT_PRESENTATIONML_SLIDE" ->
                        Decode.succeed ApplicationVndOpenxmlformatsOfficedocumentPresentationmlSlide

                    "APPLICATION_VND_OPENXMLFORMATS_OFFICEDOCUMENT_PRESENTATIONML_SLIDESHOW" ->
                        Decode.succeed ApplicationVndOpenxmlformatsOfficedocumentPresentationmlSlideshow

                    "APPLICATION_VND_OPENXMLFORMATS_OFFICEDOCUMENT_PRESENTATIONML_TEMPLATE" ->
                        Decode.succeed ApplicationVndOpenxmlformatsOfficedocumentPresentationmlTemplate

                    "APPLICATION_VND_OPENXMLFORMATS_OFFICEDOCUMENT_SPREADSHEETML_SHEET" ->
                        Decode.succeed ApplicationVndOpenxmlformatsOfficedocumentSpreadsheetmlSheet

                    "APPLICATION_VND_OPENXMLFORMATS_OFFICEDOCUMENT_SPREADSHEETML_TEMPLATE" ->
                        Decode.succeed ApplicationVndOpenxmlformatsOfficedocumentSpreadsheetmlTemplate

                    "APPLICATION_VND_OPENXMLFORMATS_OFFICEDOCUMENT_WORDPROCESSINGML_DOCUMENT" ->
                        Decode.succeed ApplicationVndOpenxmlformatsOfficedocumentWordprocessingmlDocument

                    "APPLICATION_VND_OPENXMLFORMATS_OFFICEDOCUMENT_WORDPROCESSINGML_TEMPLATE" ->
                        Decode.succeed ApplicationVndOpenxmlformatsOfficedocumentWordprocessingmlTemplate

                    "APPLICATION_WORDPERFECT" ->
                        Decode.succeed ApplicationWordperfect

                    "APPLICATION_X_7Z_COMPRESSED" ->
                        Decode.succeed ApplicationX7zCompressed

                    "APPLICATION_X_GZIP" ->
                        Decode.succeed ApplicationXGzip

                    "APPLICATION_X_TAR" ->
                        Decode.succeed ApplicationXTar

                    "APPLICATION_ZIP" ->
                        Decode.succeed ApplicationZip

                    "AUDIO_AAC" ->
                        Decode.succeed AudioAac

                    "AUDIO_FLAC" ->
                        Decode.succeed AudioFlac

                    "AUDIO_MIDI" ->
                        Decode.succeed AudioMidi

                    "AUDIO_MPEG" ->
                        Decode.succeed AudioMpeg

                    "AUDIO_OGG" ->
                        Decode.succeed AudioOgg

                    "AUDIO_WAV" ->
                        Decode.succeed AudioWav

                    "AUDIO_X_MATROSKA" ->
                        Decode.succeed AudioXMatroska

                    "AUDIO_X_MS_WAX" ->
                        Decode.succeed AudioXMsWax

                    "AUDIO_X_MS_WMA" ->
                        Decode.succeed AudioXMsWma

                    "AUDIO_X_REALAUDIO" ->
                        Decode.succeed AudioXRealaudio

                    "IMAGE_BMP" ->
                        Decode.succeed ImageBmp

                    "IMAGE_GIF" ->
                        Decode.succeed ImageGif

                    "IMAGE_HEIC" ->
                        Decode.succeed ImageHeic

                    "IMAGE_JPEG" ->
                        Decode.succeed ImageJpeg

                    "IMAGE_PNG" ->
                        Decode.succeed ImagePng

                    "IMAGE_TIFF" ->
                        Decode.succeed ImageTiff

                    "IMAGE_X_ICON" ->
                        Decode.succeed ImageXIcon

                    "TEXT_CALENDAR" ->
                        Decode.succeed TextCalendar

                    "TEXT_CSS" ->
                        Decode.succeed TextCss

                    "TEXT_CSV" ->
                        Decode.succeed TextCsv

                    "TEXT_PLAIN" ->
                        Decode.succeed TextPlain

                    "TEXT_RICHTEXT" ->
                        Decode.succeed TextRichtext

                    "TEXT_TAB_SEPARATED_VALUES" ->
                        Decode.succeed TextTabSeparatedValues

                    "TEXT_VTT" ->
                        Decode.succeed TextVtt

                    "VIDEO_3GPP" ->
                        Decode.succeed Video3gpp

                    "VIDEO_3GPP2" ->
                        Decode.succeed Video3gpp2

                    "VIDEO_AVI" ->
                        Decode.succeed VideoAvi

                    "VIDEO_DIVX" ->
                        Decode.succeed VideoDivx

                    "VIDEO_MP4" ->
                        Decode.succeed VideoMp4

                    "VIDEO_MPEG" ->
                        Decode.succeed VideoMpeg

                    "VIDEO_OGG" ->
                        Decode.succeed VideoOgg

                    "VIDEO_QUICKTIME" ->
                        Decode.succeed VideoQuicktime

                    "VIDEO_WEBM" ->
                        Decode.succeed VideoWebm

                    "VIDEO_X_FLV" ->
                        Decode.succeed VideoXFlv

                    "VIDEO_X_MATROSKA" ->
                        Decode.succeed VideoXMatroska

                    "VIDEO_X_MS_ASF" ->
                        Decode.succeed VideoXMsAsf

                    "VIDEO_X_MS_WM" ->
                        Decode.succeed VideoXMsWm

                    "VIDEO_X_MS_WMV" ->
                        Decode.succeed VideoXMsWmv

                    "VIDEO_X_MS_WMX" ->
                        Decode.succeed VideoXMsWmx

                    _ ->
                        Decode.fail ("Invalid MimeTypeEnum type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
        )
        

{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : MimeTypeEnum -> String
toString enum____ =
    case enum____ of
        ApplicationJava ->
                "APPLICATION_JAVA"


        ApplicationMsword ->
                "APPLICATION_MSWORD"


        ApplicationOctetStream ->
                "APPLICATION_OCTET_STREAM"


        ApplicationOnenote ->
                "APPLICATION_ONENOTE"


        ApplicationOxps ->
                "APPLICATION_OXPS"


        ApplicationPdf ->
                "APPLICATION_PDF"


        ApplicationRar ->
                "APPLICATION_RAR"


        ApplicationRtf ->
                "APPLICATION_RTF"


        ApplicationTtafXml ->
                "APPLICATION_TTAF_XML"


        ApplicationVndAppleKeynote ->
                "APPLICATION_VND_APPLE_KEYNOTE"


        ApplicationVndAppleNumbers ->
                "APPLICATION_VND_APPLE_NUMBERS"


        ApplicationVndApplePages ->
                "APPLICATION_VND_APPLE_PAGES"


        ApplicationVndMsAccess ->
                "APPLICATION_VND_MS_ACCESS"


        ApplicationVndMsExcel ->
                "APPLICATION_VND_MS_EXCEL"


        ApplicationVndMsExcelAddinMacroenabled12 ->
                "APPLICATION_VND_MS_EXCEL_ADDIN_MACROENABLED_12"


        ApplicationVndMsExcelSheetBinaryMacroenabled12 ->
                "APPLICATION_VND_MS_EXCEL_SHEET_BINARY_MACROENABLED_12"


        ApplicationVndMsExcelSheetMacroenabled12 ->
                "APPLICATION_VND_MS_EXCEL_SHEET_MACROENABLED_12"


        ApplicationVndMsExcelTemplateMacroenabled12 ->
                "APPLICATION_VND_MS_EXCEL_TEMPLATE_MACROENABLED_12"


        ApplicationVndMsPowerpoint ->
                "APPLICATION_VND_MS_POWERPOINT"


        ApplicationVndMsPowerpointAddinMacroenabled12 ->
                "APPLICATION_VND_MS_POWERPOINT_ADDIN_MACROENABLED_12"


        ApplicationVndMsPowerpointPresentationMacroenabled12 ->
                "APPLICATION_VND_MS_POWERPOINT_PRESENTATION_MACROENABLED_12"


        ApplicationVndMsPowerpointSlideshowMacroenabled12 ->
                "APPLICATION_VND_MS_POWERPOINT_SLIDESHOW_MACROENABLED_12"


        ApplicationVndMsPowerpointSlideMacroenabled12 ->
                "APPLICATION_VND_MS_POWERPOINT_SLIDE_MACROENABLED_12"


        ApplicationVndMsPowerpointTemplateMacroenabled12 ->
                "APPLICATION_VND_MS_POWERPOINT_TEMPLATE_MACROENABLED_12"


        ApplicationVndMsProject ->
                "APPLICATION_VND_MS_PROJECT"


        ApplicationVndMsWordDocumentMacroenabled12 ->
                "APPLICATION_VND_MS_WORD_DOCUMENT_MACROENABLED_12"


        ApplicationVndMsWordTemplateMacroenabled12 ->
                "APPLICATION_VND_MS_WORD_TEMPLATE_MACROENABLED_12"


        ApplicationVndMsWrite ->
                "APPLICATION_VND_MS_WRITE"


        ApplicationVndMsXpsdocument ->
                "APPLICATION_VND_MS_XPSDOCUMENT"


        ApplicationVndOasisOpendocumentChart ->
                "APPLICATION_VND_OASIS_OPENDOCUMENT_CHART"


        ApplicationVndOasisOpendocumentDatabase ->
                "APPLICATION_VND_OASIS_OPENDOCUMENT_DATABASE"


        ApplicationVndOasisOpendocumentFormula ->
                "APPLICATION_VND_OASIS_OPENDOCUMENT_FORMULA"


        ApplicationVndOasisOpendocumentGraphics ->
                "APPLICATION_VND_OASIS_OPENDOCUMENT_GRAPHICS"


        ApplicationVndOasisOpendocumentPresentation ->
                "APPLICATION_VND_OASIS_OPENDOCUMENT_PRESENTATION"


        ApplicationVndOasisOpendocumentSpreadsheet ->
                "APPLICATION_VND_OASIS_OPENDOCUMENT_SPREADSHEET"


        ApplicationVndOasisOpendocumentText ->
                "APPLICATION_VND_OASIS_OPENDOCUMENT_TEXT"


        ApplicationVndOpenxmlformatsOfficedocumentPresentationmlPresentation ->
                "APPLICATION_VND_OPENXMLFORMATS_OFFICEDOCUMENT_PRESENTATIONML_PRESENTATION"


        ApplicationVndOpenxmlformatsOfficedocumentPresentationmlSlide ->
                "APPLICATION_VND_OPENXMLFORMATS_OFFICEDOCUMENT_PRESENTATIONML_SLIDE"


        ApplicationVndOpenxmlformatsOfficedocumentPresentationmlSlideshow ->
                "APPLICATION_VND_OPENXMLFORMATS_OFFICEDOCUMENT_PRESENTATIONML_SLIDESHOW"


        ApplicationVndOpenxmlformatsOfficedocumentPresentationmlTemplate ->
                "APPLICATION_VND_OPENXMLFORMATS_OFFICEDOCUMENT_PRESENTATIONML_TEMPLATE"


        ApplicationVndOpenxmlformatsOfficedocumentSpreadsheetmlSheet ->
                "APPLICATION_VND_OPENXMLFORMATS_OFFICEDOCUMENT_SPREADSHEETML_SHEET"


        ApplicationVndOpenxmlformatsOfficedocumentSpreadsheetmlTemplate ->
                "APPLICATION_VND_OPENXMLFORMATS_OFFICEDOCUMENT_SPREADSHEETML_TEMPLATE"


        ApplicationVndOpenxmlformatsOfficedocumentWordprocessingmlDocument ->
                "APPLICATION_VND_OPENXMLFORMATS_OFFICEDOCUMENT_WORDPROCESSINGML_DOCUMENT"


        ApplicationVndOpenxmlformatsOfficedocumentWordprocessingmlTemplate ->
                "APPLICATION_VND_OPENXMLFORMATS_OFFICEDOCUMENT_WORDPROCESSINGML_TEMPLATE"


        ApplicationWordperfect ->
                "APPLICATION_WORDPERFECT"


        ApplicationX7zCompressed ->
                "APPLICATION_X_7Z_COMPRESSED"


        ApplicationXGzip ->
                "APPLICATION_X_GZIP"


        ApplicationXTar ->
                "APPLICATION_X_TAR"


        ApplicationZip ->
                "APPLICATION_ZIP"


        AudioAac ->
                "AUDIO_AAC"


        AudioFlac ->
                "AUDIO_FLAC"


        AudioMidi ->
                "AUDIO_MIDI"


        AudioMpeg ->
                "AUDIO_MPEG"


        AudioOgg ->
                "AUDIO_OGG"


        AudioWav ->
                "AUDIO_WAV"


        AudioXMatroska ->
                "AUDIO_X_MATROSKA"


        AudioXMsWax ->
                "AUDIO_X_MS_WAX"


        AudioXMsWma ->
                "AUDIO_X_MS_WMA"


        AudioXRealaudio ->
                "AUDIO_X_REALAUDIO"


        ImageBmp ->
                "IMAGE_BMP"


        ImageGif ->
                "IMAGE_GIF"


        ImageHeic ->
                "IMAGE_HEIC"


        ImageJpeg ->
                "IMAGE_JPEG"


        ImagePng ->
                "IMAGE_PNG"


        ImageTiff ->
                "IMAGE_TIFF"


        ImageXIcon ->
                "IMAGE_X_ICON"


        TextCalendar ->
                "TEXT_CALENDAR"


        TextCss ->
                "TEXT_CSS"


        TextCsv ->
                "TEXT_CSV"


        TextPlain ->
                "TEXT_PLAIN"


        TextRichtext ->
                "TEXT_RICHTEXT"


        TextTabSeparatedValues ->
                "TEXT_TAB_SEPARATED_VALUES"


        TextVtt ->
                "TEXT_VTT"


        Video3gpp ->
                "VIDEO_3GPP"


        Video3gpp2 ->
                "VIDEO_3GPP2"


        VideoAvi ->
                "VIDEO_AVI"


        VideoDivx ->
                "VIDEO_DIVX"


        VideoMp4 ->
                "VIDEO_MP4"


        VideoMpeg ->
                "VIDEO_MPEG"


        VideoOgg ->
                "VIDEO_OGG"


        VideoQuicktime ->
                "VIDEO_QUICKTIME"


        VideoWebm ->
                "VIDEO_WEBM"


        VideoXFlv ->
                "VIDEO_X_FLV"


        VideoXMatroska ->
                "VIDEO_X_MATROSKA"


        VideoXMsAsf ->
                "VIDEO_X_MS_ASF"


        VideoXMsWm ->
                "VIDEO_X_MS_WM"


        VideoXMsWmv ->
                "VIDEO_X_MS_WMV"


        VideoXMsWmx ->
                "VIDEO_X_MS_WMX"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe MimeTypeEnum
fromString enumString____ =
    case enumString____ of
        "APPLICATION_JAVA" ->
                Just ApplicationJava


        "APPLICATION_MSWORD" ->
                Just ApplicationMsword


        "APPLICATION_OCTET_STREAM" ->
                Just ApplicationOctetStream


        "APPLICATION_ONENOTE" ->
                Just ApplicationOnenote


        "APPLICATION_OXPS" ->
                Just ApplicationOxps


        "APPLICATION_PDF" ->
                Just ApplicationPdf


        "APPLICATION_RAR" ->
                Just ApplicationRar


        "APPLICATION_RTF" ->
                Just ApplicationRtf


        "APPLICATION_TTAF_XML" ->
                Just ApplicationTtafXml


        "APPLICATION_VND_APPLE_KEYNOTE" ->
                Just ApplicationVndAppleKeynote


        "APPLICATION_VND_APPLE_NUMBERS" ->
                Just ApplicationVndAppleNumbers


        "APPLICATION_VND_APPLE_PAGES" ->
                Just ApplicationVndApplePages


        "APPLICATION_VND_MS_ACCESS" ->
                Just ApplicationVndMsAccess


        "APPLICATION_VND_MS_EXCEL" ->
                Just ApplicationVndMsExcel


        "APPLICATION_VND_MS_EXCEL_ADDIN_MACROENABLED_12" ->
                Just ApplicationVndMsExcelAddinMacroenabled12


        "APPLICATION_VND_MS_EXCEL_SHEET_BINARY_MACROENABLED_12" ->
                Just ApplicationVndMsExcelSheetBinaryMacroenabled12


        "APPLICATION_VND_MS_EXCEL_SHEET_MACROENABLED_12" ->
                Just ApplicationVndMsExcelSheetMacroenabled12


        "APPLICATION_VND_MS_EXCEL_TEMPLATE_MACROENABLED_12" ->
                Just ApplicationVndMsExcelTemplateMacroenabled12


        "APPLICATION_VND_MS_POWERPOINT" ->
                Just ApplicationVndMsPowerpoint


        "APPLICATION_VND_MS_POWERPOINT_ADDIN_MACROENABLED_12" ->
                Just ApplicationVndMsPowerpointAddinMacroenabled12


        "APPLICATION_VND_MS_POWERPOINT_PRESENTATION_MACROENABLED_12" ->
                Just ApplicationVndMsPowerpointPresentationMacroenabled12


        "APPLICATION_VND_MS_POWERPOINT_SLIDESHOW_MACROENABLED_12" ->
                Just ApplicationVndMsPowerpointSlideshowMacroenabled12


        "APPLICATION_VND_MS_POWERPOINT_SLIDE_MACROENABLED_12" ->
                Just ApplicationVndMsPowerpointSlideMacroenabled12


        "APPLICATION_VND_MS_POWERPOINT_TEMPLATE_MACROENABLED_12" ->
                Just ApplicationVndMsPowerpointTemplateMacroenabled12


        "APPLICATION_VND_MS_PROJECT" ->
                Just ApplicationVndMsProject


        "APPLICATION_VND_MS_WORD_DOCUMENT_MACROENABLED_12" ->
                Just ApplicationVndMsWordDocumentMacroenabled12


        "APPLICATION_VND_MS_WORD_TEMPLATE_MACROENABLED_12" ->
                Just ApplicationVndMsWordTemplateMacroenabled12


        "APPLICATION_VND_MS_WRITE" ->
                Just ApplicationVndMsWrite


        "APPLICATION_VND_MS_XPSDOCUMENT" ->
                Just ApplicationVndMsXpsdocument


        "APPLICATION_VND_OASIS_OPENDOCUMENT_CHART" ->
                Just ApplicationVndOasisOpendocumentChart


        "APPLICATION_VND_OASIS_OPENDOCUMENT_DATABASE" ->
                Just ApplicationVndOasisOpendocumentDatabase


        "APPLICATION_VND_OASIS_OPENDOCUMENT_FORMULA" ->
                Just ApplicationVndOasisOpendocumentFormula


        "APPLICATION_VND_OASIS_OPENDOCUMENT_GRAPHICS" ->
                Just ApplicationVndOasisOpendocumentGraphics


        "APPLICATION_VND_OASIS_OPENDOCUMENT_PRESENTATION" ->
                Just ApplicationVndOasisOpendocumentPresentation


        "APPLICATION_VND_OASIS_OPENDOCUMENT_SPREADSHEET" ->
                Just ApplicationVndOasisOpendocumentSpreadsheet


        "APPLICATION_VND_OASIS_OPENDOCUMENT_TEXT" ->
                Just ApplicationVndOasisOpendocumentText


        "APPLICATION_VND_OPENXMLFORMATS_OFFICEDOCUMENT_PRESENTATIONML_PRESENTATION" ->
                Just ApplicationVndOpenxmlformatsOfficedocumentPresentationmlPresentation


        "APPLICATION_VND_OPENXMLFORMATS_OFFICEDOCUMENT_PRESENTATIONML_SLIDE" ->
                Just ApplicationVndOpenxmlformatsOfficedocumentPresentationmlSlide


        "APPLICATION_VND_OPENXMLFORMATS_OFFICEDOCUMENT_PRESENTATIONML_SLIDESHOW" ->
                Just ApplicationVndOpenxmlformatsOfficedocumentPresentationmlSlideshow


        "APPLICATION_VND_OPENXMLFORMATS_OFFICEDOCUMENT_PRESENTATIONML_TEMPLATE" ->
                Just ApplicationVndOpenxmlformatsOfficedocumentPresentationmlTemplate


        "APPLICATION_VND_OPENXMLFORMATS_OFFICEDOCUMENT_SPREADSHEETML_SHEET" ->
                Just ApplicationVndOpenxmlformatsOfficedocumentSpreadsheetmlSheet


        "APPLICATION_VND_OPENXMLFORMATS_OFFICEDOCUMENT_SPREADSHEETML_TEMPLATE" ->
                Just ApplicationVndOpenxmlformatsOfficedocumentSpreadsheetmlTemplate


        "APPLICATION_VND_OPENXMLFORMATS_OFFICEDOCUMENT_WORDPROCESSINGML_DOCUMENT" ->
                Just ApplicationVndOpenxmlformatsOfficedocumentWordprocessingmlDocument


        "APPLICATION_VND_OPENXMLFORMATS_OFFICEDOCUMENT_WORDPROCESSINGML_TEMPLATE" ->
                Just ApplicationVndOpenxmlformatsOfficedocumentWordprocessingmlTemplate


        "APPLICATION_WORDPERFECT" ->
                Just ApplicationWordperfect


        "APPLICATION_X_7Z_COMPRESSED" ->
                Just ApplicationX7zCompressed


        "APPLICATION_X_GZIP" ->
                Just ApplicationXGzip


        "APPLICATION_X_TAR" ->
                Just ApplicationXTar


        "APPLICATION_ZIP" ->
                Just ApplicationZip


        "AUDIO_AAC" ->
                Just AudioAac


        "AUDIO_FLAC" ->
                Just AudioFlac


        "AUDIO_MIDI" ->
                Just AudioMidi


        "AUDIO_MPEG" ->
                Just AudioMpeg


        "AUDIO_OGG" ->
                Just AudioOgg


        "AUDIO_WAV" ->
                Just AudioWav


        "AUDIO_X_MATROSKA" ->
                Just AudioXMatroska


        "AUDIO_X_MS_WAX" ->
                Just AudioXMsWax


        "AUDIO_X_MS_WMA" ->
                Just AudioXMsWma


        "AUDIO_X_REALAUDIO" ->
                Just AudioXRealaudio


        "IMAGE_BMP" ->
                Just ImageBmp


        "IMAGE_GIF" ->
                Just ImageGif


        "IMAGE_HEIC" ->
                Just ImageHeic


        "IMAGE_JPEG" ->
                Just ImageJpeg


        "IMAGE_PNG" ->
                Just ImagePng


        "IMAGE_TIFF" ->
                Just ImageTiff


        "IMAGE_X_ICON" ->
                Just ImageXIcon


        "TEXT_CALENDAR" ->
                Just TextCalendar


        "TEXT_CSS" ->
                Just TextCss


        "TEXT_CSV" ->
                Just TextCsv


        "TEXT_PLAIN" ->
                Just TextPlain


        "TEXT_RICHTEXT" ->
                Just TextRichtext


        "TEXT_TAB_SEPARATED_VALUES" ->
                Just TextTabSeparatedValues


        "TEXT_VTT" ->
                Just TextVtt


        "VIDEO_3GPP" ->
                Just Video3gpp


        "VIDEO_3GPP2" ->
                Just Video3gpp2


        "VIDEO_AVI" ->
                Just VideoAvi


        "VIDEO_DIVX" ->
                Just VideoDivx


        "VIDEO_MP4" ->
                Just VideoMp4


        "VIDEO_MPEG" ->
                Just VideoMpeg


        "VIDEO_OGG" ->
                Just VideoOgg


        "VIDEO_QUICKTIME" ->
                Just VideoQuicktime


        "VIDEO_WEBM" ->
                Just VideoWebm


        "VIDEO_X_FLV" ->
                Just VideoXFlv


        "VIDEO_X_MATROSKA" ->
                Just VideoXMatroska


        "VIDEO_X_MS_ASF" ->
                Just VideoXMsAsf


        "VIDEO_X_MS_WM" ->
                Just VideoXMsWm


        "VIDEO_X_MS_WMV" ->
                Just VideoXMsWmv


        "VIDEO_X_MS_WMX" ->
                Just VideoXMsWmx

        _ ->
                Nothing
