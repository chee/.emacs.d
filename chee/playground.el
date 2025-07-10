(defun chee/list-of-zotero-pdfs nil
  (sqlite-select (sqlite-open "~/Zotero/zotero.sqlite")
    "SELECT CASE WHEN itemAttachments.path LIKE 'storage:%' THEN
    printf(
      '%s/storage/%s/%s',
      (SELECT value FROM settings
        WHERE setting = 'baseAttachmentPath'
        OR key = 'baseAttachmentPath'),
      items.key,
      REPLACE(itemAttachments.path, 'storage:', ''))
      WHEN itemAttachments.path LIKE 'attachments:%' THEN
        printf(
          '%s/%s',
          (SELECT value FROM settings
             WHERE setting = 'baseAttachmentPath'
             OR key = 'baseAttachmentPath'),
             REPLACE(itemAttachments.path, 'attachments:', '')
       )
      ELSE itemAttachments.path
   END as full_path
   FROM items
   JOIN itemAttachments ON items.itemID = itemAttachments.itemID
   WHERE (itemAttachments.contentType = 'application/pdf'
      OR itemAttachments.path LIKE '%.pdf')
 AND itemAttachments.linkMode IN (0, 1, 2);"))


(defun chee/select-zotero-pdf nil
  (completing-read
    "Select a pdf from zotero"
    (chee/list-of-zotero-pdfs)))

(find-file (string-join `("~/Zotero" ,(chee/select-zotero-pdf))))
