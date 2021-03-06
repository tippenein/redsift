{-# LANGUAGE OverloadedStrings #-}

module Redsift.Mail where

import Data.String.Conversions
import Network.Mail.Mime       (Mail (..), plainPart, renderSendMail)
import System.IO

import Redsift.Config


sendCSVExportMail :: EmailConfig -> Address -> String -> IO ()
sendCSVExportMail config recipient url =
    let subj = "Your Redsift Export"
        body = "Hi Redsift User, \n\n Here's your exported data URL:\n" ++ url ++ "\n\nThanks,\n\n This email was sent on behalf of Zalora DataScience team"
    in do
        hPutStrLn stderr $ "Sending to " ++ cs (addressEmail recipient) ++ ":\n" ++ url
        sendMail config recipient subj body

sendMail :: EmailConfig -> Address -> String -> String -> IO ()
sendMail (EmailConfig from) recipient subj body =
    renderSendMail Mail {
            mailFrom = from,
            mailTo = [recipient],
            mailCc = [],
            mailBcc = [],
            mailHeaders = [("Subject", cs subj)],
            mailParts = [[plainPart (cs body)]]
          }
