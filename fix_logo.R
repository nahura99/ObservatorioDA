library(magick)

# 1. Start with the image
img <- image_read("www/logo_oda_v2.png")
info <- image_info(img)
w <- info$width
h <- info$height

# 2. Make white transparent (the whole thing)
img_transparent <- image_transparent(img, "white", fuzz = 15)

# 3. Create a white circle on black background to restore the white in the central circle
# We use this as a mask or just as an image.
# Actually, let's create a circle image and put it "behind" or "on top".
# Better: use the original image and trim everything outside the circle radius.
mask <- image_blank(w, h, "black")
mask <- image_draw(mask)
symbols(w/2, h/2, circles = w/2 - 2, bg = "white", add = TRUE, inches = FALSE)
dev.off()

# Apply the mask: only pixels inside the radius stay.
# AND we already made white transparent in step 2.
# So we composite: original image + mask as opacity.
img_final <- image_composite(img_transparent, mask, operator = "copyopacity")

# Final flood fill to ensure zero-fuzz white didn't remain at corners
img_final <- image_fill(img_final, "transparent", point = "+0+0", fuzz = 0)

# Save
image_write(img_final, "www/logo_oda_v2.png", format = "png")
cat("Final robust transparency fix with mask and flood fill applied.\n")
