import { google } from "googleapis";
import { createTransport, TransportOptions } from "nodemailer";
import process from "process";

const OAuth2 = google.auth.OAuth2;

// Dane uwierzytelniające (z pliku credentials.json)
const clientId = import.meta.env.VITE_GMAIL_CLIENT_ID; // Zmienne środowiskowe
const clientSecret = import.meta.env.VITE_GMAIL_CLIENT_SECRET;
const refreshToken = import.meta.env.VITE_GMAIL_REFRESH_TOKEN; // Zmienne środowiskowe - uzyskany podczas autoryzacji

console.log("clientId: ", clientId);

const oauth2Client = new OAuth2(
  clientId,
  clientSecret,
  "https://developers.google.com/oauthplayground"
); // Redirect URL - możesz użyć OAuth Playground

oauth2Client.setCredentials({
  refresh_token: refreshToken,
});

interface EmailOptions {
  from: string;
  to: string;
  subject: string;
  text?: string;
  html?: string;
}

const accessToken = oauth2Client.getAccessToken();

// Konfiguracja transportu -  dane uwierzytelniające do twojego serwera pocztowego
const transporter = createTransport({
  service: "gmail",
  auth: {
    type: "OAuth2",
    user: import.meta.env.VITE_GMAIL_USER, // Twój email
    accessToken: accessToken,
    clientId: clientId,
    clientSecret: clientSecret,
    refreshToken: refreshToken,
  },
} as TransportOptions);

async function sendEmail(options: EmailOptions) {
  try {
    const info = await transporter.sendMail(options);
    console.log("Message sent: %s", info.messageId);
  } catch (error) {
    console.error("Error sending email:", error);
  }
}

export { sendEmail };
export type { EmailOptions };
